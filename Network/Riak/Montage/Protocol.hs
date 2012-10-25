module Network.Riak.Montage.Protocol where

import System.ZMQ
import System.UUID.V4 (uuid)
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as BW
import qualified Data.ByteString.Char8 as S
import Control.Exception (try, SomeException)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)
import Text.ProtocolBuffers.Basic (uFromString)

import Network.Riak.Montage.Util

import Network.StatsWeb (Stats)

import Network.Riak.Montage.Proto.Montage.MontageEnvelope as ME
import Network.Riak.Montage.Proto.Montage.MontageWireMessages
import Network.Riak.Montage.Proto.Montage.MontageError
import Network.Riak.Montage.Types
import Network.Riak.Montage.Process (processRequest,
                                    serializeResponse, ConcurrentState(..))


type ZmqHandler = (S.ByteString -> (BW.ByteString -> IO ()) -> IO ())

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
runZmqRpcWithContext ctx binda call = do
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
                _ <- forkIO $ call m (zmqRpcReply ctx inproc zid)
                return ()
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

serveMontageZmq :: (MontageRiakValue r) =>
                   (MontageEnvelope -> ChainCommand r) ->
                   String -> ConcurrentState -> LogCallback ->
                   PoolChooser -> Stats -> IO ()
serveMontageZmq generator runOn state logger chooser' stats = do
    runZmqRpc runOn wrapMontage
  where
    wrapMontage m cb = do
        case messageGet $ sTl m of
            Right (env, x) | B.length x == 0 -> do
                res <- try $ do
                    let !cmd = generator env
                    fmap (serializeResponse env) $ processRequest state logger chooser' cmd stats
                case res of
                    Left (e :: SomeException) -> returnError (show e) $ msgid env
                    Right outenv -> cb $  messagePut outenv

            _ -> returnError "Failed to decode MontageEnvelope" Nothing
      where
        returnError err msgid' = do
            logError err
            cb $ messagePut $ MontageEnvelope {
                  mtype = MONTAGE_ERROR
                , msg = messagePut $ MontageError (uFromString err)
                , msgid = msgid'
                }
