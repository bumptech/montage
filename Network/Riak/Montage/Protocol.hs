module Network.Riak.Montage.Protocol where

import System.ZMQ
import System.UUID.V4 (uuid)
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (takeMVar, putMVar)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as BW
import qualified Data.ByteString.Char8 as S
import Control.Exception (try, SomeException)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)
import Text.ProtocolBuffers.Basic (uFromString)
import Data.Aeson (object, (.=))

import Network.Riak.Montage.Util

import Network.StatsWeb (Stats)

import Network.Riak.Montage.Proto.Montage.MontageEnvelope as ME
import Network.Riak.Montage.Proto.Montage.MontageWireMessages
import Network.Riak.Montage.Proto.Montage.MontageError
import Network.Riak.Montage.Types
import Network.Riak.Montage.Process (processRequest,
                                    serializeResponse)

--temp
import qualified Bump.LRUCache.IO as LRU


type ZmqHandler = (S.ByteString -> ZmqCallback -> IO ())

type ZmqCallback = (BW.ByteString -> IO ())

runZmqRpc :: String
          -> ZmqHandler
          -> IO ()
runZmqRpc bindSpec call = do
    withContext 1 (\c ->
        runZmqRpcWithContext c bindSpec call)

runZmqRpcWithContext :: Context
                     -> String
                     -> ZmqHandler
                     -> IO ()
runZmqRpcWithContext ctx binda serve = do
    withSocket ctx Router (\s -> do
        rand <- uuid
        let inproc = "inproc://" ++ (show rand)
        bind s binda
        bind s inproc
        forever $ do
            zid <- receive s []
            _ <- receive s []
            m <- receive s []
            more <- moreToReceive s
            if more
            then do  -- forward
                fwid <- receive s []
                send s fwid [SndMore]
                send s "" [SndMore]
                send s m []
            else do -- call
                serve m (zmqRpcReply ctx inproc zid)
        )

zmqRpcReply :: Context
            -> String        -- inproc
            -> S.ByteString  -- sent id
            -> BW.ByteString  -- out message
            -> IO ()
zmqRpcReply c inproc retid out = do
    withSocket c Req (\s -> do
        connect s inproc
        send' s out [SndMore]
        send s retid []
        )

serveMontageZmq :: (MontageRiakValue r) => MVar (S.ByteString, Maybe [ZmqCallback]) -> String -> IO ()
serveMontageZmq queueAny runOn = do
    current <- LRU.newLRU 1000
    runZmqRpc runOn (serveMontage queueAny current)

serveMontage queueAny current m cb = do
        mres <- LRU.lookup current m
        case mres of
            Just pipe -> pipeline cb m pipe
            Nothing -> routeToAnyone cb m
  where
    pipeline cb m pipe = do
        pipestate <- takeMVar pipe
        case pipestate of
            Just ps -> putMVar pipe $ Just (cb:ps)
            Nothing -> routeToAnyone m current

    routeToAnyone cb m = do
        pipe <- newMVar (Just [cb])
        LRU.insert current m pipe
        putMVar queueAny (m,pipe)

processLoop queueAny generate runOn logCB chooser' stats requestTimeout' readOnly' logCommands' = do
    forever $ do
        (item, pipe) <- takeMVar queueAny
        pipe' <- takeMVar pipe
        putMVar pipe Nothing
        m <- wrapMontage item
        mapM_ (\cb -> cb m) pipe'
  where
    wrapMontage m = do
        case messageGet $ sTl m of
            Right (env, x) | B.length x == 0 -> do
                res <- try $ do
                    let !cmd = generate env
                    fmap (serializeResponse env) $ processRequest chooser' cmd stats requestTimeout' readOnly' logCommands'
                case res of
                    Left (e :: SomeException) -> returnError (show e) $ msgid env
                    Right outenv -> return . messagePut $ outenv

            _ -> returnError "Failed to decode MontageEnvelope" Nothing
      where
        returnError err msgid' = do
            logError err
            logCB "EXCEPTION" Nothing $ object ["error" .=  err ]
            return . messagePut $ MontageEnvelope {
                  mtype = MONTAGE_ERROR
                , msg = messagePut $ MontageError (uFromString err)
                , msgid = msgid'
                }
