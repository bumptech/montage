module Network.Riak.Montage.Process where

import Control.Monad (void, when)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar, MVar)
import Control.Exception (try, throw, SomeException)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)
import System.Timeout (timeout)
import Data.Maybe (fromJust)

import qualified Network.Riak.Types as RT
import Text.ProtocolBuffers.Basic (toUtf8, utf8)
import qualified Data.Text.Encoding as E
import Data.Foldable (toList)
import qualified Data.ListLike as LL
import qualified Data.Text.Lazy as TL
import Data.Text.Format

import qualified Network.Riak.Value as V
import qualified Network.Riak.Content as C

import Network.Riak.Montage.Proto.Montage.MontageEnvelope as ME
import Network.Riak.Montage.Proto.Montage.MontageWireMessages

import Network.StatsWeb (Stats, incCounter)

import Network.Riak.Montage.Types
import Network.Riak.Montage.Backend
import Network.Riak.Montage.Commands
import Network.Riak.Montage.Util
import qualified Network.Riak.Montage.Proto.Montage.MontageGet as MG
import qualified Network.Riak.Montage.Proto.Montage.MontageGetMany as MGM
import qualified Network.Riak.Montage.Proto.Montage.MontageGetReference as MGR
import qualified Network.Riak.Montage.Proto.Montage.MontagePut as MP
import qualified Network.Riak.Montage.Proto.Montage.MontagePutMany as MPM
import qualified Network.Riak.Montage.Proto.Montage.MontageObject as MO
import qualified Network.Riak.Montage.Proto.Montage.MontageCommand as MC
import qualified Network.Riak.Montage.Proto.Montage.MontageCommandResponse as MCR
import qualified Network.Riak.Montage.Proto.Montage.MontageDelete as MD

runWithTimeout :: Int -> IO a -> IO a
runWithTimeout requestTimeout' action = do
    mr <- timeout requestTimeout action
    case mr of
        Just r -> do
            return r
        Nothing -> do
            error "montage request timeout!"
  where
    requestTimeout = requestTimeout' * 1000000

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _) = error "fromRight got Left!"

generateRequest :: (MontageRiakValue r) => MontageEnvelope -> ChainCommand r
generateRequest (MontageEnvelope MONTAGE_GET inp _) =
    ChainGet buck key Nothing
  where
    buck = MG.bucket obj
    key = MG.key obj
    obj = (fst . fromRight $ messageGet $ inp) :: MG.MontageGet

generateRequest (MontageEnvelope MONTAGE_GET_MANY inp _) =
    ChainGetMany gets Nothing Nothing
  where
    wrap = (fst . fromRight $ messageGet $ inp) :: MGM.MontageGetMany
    subs = MGM.gets wrap
    gets = toList $ fmap makeGet subs

    makeGet g = (buck, key)
        where
            buck = MG.bucket g
            key = MG.key g

generateRequest (MontageEnvelope MONTAGE_COMMAND inp _) =
    ChainCustom command arg
  where
    command = E.decodeUtf8 $ lTs $ utf8 $ MC.command obj
    arg = MC.argument obj
    obj = (fst . fromRight $ messageGet $ inp) :: MC.MontageCommand

generateRequest (MontageEnvelope MONTAGE_PUT inp _) =
    ChainPut vclock buck key dat Nothing
  where
    obj = MP.object wrap
    vclock = MO.vclock obj
    buck = MO.bucket obj
    key = MO.key obj
    dat = fromJust $ V.fromContent buck $ C.empty { C.value = MO.data' obj }
    wrap = (fst . fromRight $ messageGet $ inp) :: MP.MontagePut

generateRequest (MontageEnvelope MONTAGE_PUT_MANY inp _) =
    ChainPutMany puts Nothing
  where
    pb = fst $ fromRight $ messageGet inp
    puts = toList $ fmap makePut $ MPM.objects pb
    makePut g = (MO.vclock g, buck, MO.key g, dat)
      where
        buck = MO.bucket g
        dat = fromJust $ V.fromContent buck $ C.empty { C.value = MO.data' g }

generateRequest (MontageEnvelope MONTAGE_GET_REFERENCE inp _) =
    ChainReference buck key targets
  where
    buck = MGR.bucket wrap
    key = MGR.key wrap
    targets = LL.toList $ MGR.target_buckets wrap
    wrap = (fst . fromRight $ messageGet $ inp) :: MGR.MontageGetReference

generateRequest (MontageEnvelope MONTAGE_DELETE inp _) =
    ChainDelete buck key Nothing
  where
    buck = MD.bucket obj
    key = MD.key obj
    obj = (fst . fromRight $ messageGet $ inp) :: MD.MontageDelete

generateRequest (MontageEnvelope MONTAGE_GET_RESPONSE _ _) = error "MONTAGE_GET_RESPONSE is reserved for responses from montage"
generateRequest (MontageEnvelope MONTAGE_COMMAND_RESPONSE _ _) = error "MONTAGE_COMMAND_RESPONSE is reserved for responses from montage"
generateRequest (MontageEnvelope MONTAGE_PUT_RESPONSE _ _) = error "MONTAGE_PUT_RESPONSE is reserved for responses from montage"
generateRequest (MontageEnvelope MONTAGE_PUT_MANY_RESPONSE _ _) = error "MONTAGE_PUT_MANY_RESPONSE is reserved for responses from montage"
generateRequest (MontageEnvelope MONTAGE_ERROR _ _) = error "MONTAGE_ERROR is reserved for responses from montage"
generateRequest (MontageEnvelope MONTAGE_DELETE_RESPONSE _ _) = error "MONTAGE_DELETE_RESPONSE is reserved for responses from montage"
generateRequest (MontageEnvelope DEPRICATED_MONTAGE_SET_REFERENCE _ _) = error "DEPRICATED_MONTAGE_SET_REFERENCE is deprecated!"

statsChainCustom :: (MontageRiakValue r) => ChainCommand r -> Stats -> IO ()
statsChainCustom (ChainCustom cmd _) stats = incCounter (TL.toStrict $ format "requests.custom[type={}]" (Only cmd)) stats
statsChainCustom _ _ = return ()

processRequest :: (MontageRiakValue r) => PoolChooser -> ChainCommand r -> Stats -> Int -> Bool -> Bool -> IO CommandResponse
processRequest chooser' cmd stats requestTimeout' readOnly' logCommands' = do
    when (readOnly' && (not $ isRead cmd)) $
      error "Non-read request issued to read-only montage"

    when (logCommands') $
      logError $ "Running command " ++ show cmd

    runWithTimeout requestTimeout' (processRequest' chooser' cmd stats)

processRequest' :: (MontageRiakValue r) => PoolChooser -> ChainCommand r -> Stats -> IO CommandResponse
processRequest' chooser' cmd stats = do
    statsChainCustom cmd stats
    let !step = exec cmd
    case step of
        IterationRiakCommand cmds callback -> do
            rs <- runBackendCommands chooser' stats cmds
            let !cmd' = callback rs
            processRequest' chooser' cmd' stats
        IterationRiakCommandIO cmds callback -> do
            rs <- runBackendCommands chooser' stats cmds
            !cmd' <- callback rs
            processRequest' chooser' cmd' stats
        IterationResponse final -> return final
        ChainIterationIO ioCmd -> do
            cmd' <- ioCmd
            processRequest' chooser' cmd' stats

runBackendCommands :: (MontageRiakValue r) => PoolChooser -> Stats -> [RiakRequest r] -> IO [RiakResponse r]
runBackendCommands chooser' stats rs = do
    waits <- mapM (runBackendCommand chooser' stats) rs
    results <- mapM takeMVar waits
    return $ map parseResponse results
  where
    parseResponse :: (MontageRiakValue r) => Either SomeException (RiakResponse r) -> RiakResponse r
    parseResponse (Left e) = throw e
    parseResponse (Right res) = res

runBackendCommand' :: (MontageRiakValue r) => IO (RiakResponse r) -> IO (MVar (Either SomeException (RiakResponse r)))
runBackendCommand' f = do
    wait <- newEmptyMVar
    void $ forkIO $ try f >>= putMVar wait
    return wait

runBackendCommand :: (MontageRiakValue r) => PoolChooser -> Stats -> RiakRequest r -> IO (MVar (Either SomeException (RiakResponse r)))
runBackendCommand chooser' stats (RiakGet buck key) =
    runBackendCommand' $ doGet stats buck key chooser' $ opts $ getPB buck

runBackendCommand chooser' _ (RiakPut mclock buck key value) =
    runBackendCommand' $ doPut buck key mclock value chooser'

runBackendCommand chooser' _ (RiakDelete buck key) =
    runBackendCommand' $ doDelete buck key chooser'

serializeResponse :: MontageEnvelope -> CommandResponse -> MontageEnvelope
serializeResponse env (ResponseProtobuf code proto) =
    env {mtype=code, msg=messagePut proto}
serializeResponse env (ResponseCustom s arg) =
    env {mtype=MONTAGE_COMMAND_RESPONSE,
         msg=messagePut (MCR.MontageCommandResponse (fromRight $ toUtf8 $ sTl $ E.encodeUtf8 s) arg)
    }
