module Network.Riak.Montage.Protocol where

import System.ZMQ
import System.UUID.V4 (uuid)
import Data.Monoid (mconcat)
import Control.Applicative
import Control.Monad (forever, liftM, replicateM, when)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO)
import qualified Data.Attoparsec as Atto
import qualified Data.Attoparsec.Binary as AttoB
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as BW
import qualified Data.ByteString.Char8 as S
import Data.Word (Word32)
import Control.Exception (try, SomeException)
import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Word (fromWord32le)
import Blaze.ByteString.Builder.ByteString (copyLazyByteString)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)
import Text.ProtocolBuffers.Basic (Utf8(..), utf8, toUtf8, uFromString)

import Network.Riak.Montage.Util

import Network.StatsWeb (Stats, incCounter)

import Network.Riak.Montage.Proto.Montage.MontageEnvelope as ME
import Network.Riak.Montage.Proto.Montage.MontageWireMessages
import Network.Riak.Montage.Proto.Montage.MontageError
import Network.Riak.Montage.Types
import Network.Riak.Montage.Process (generateRequest, processRequest,
                                    serializeResponse, ConcurrentState(..))


type ZmqHandler = (S.ByteString -> (BW.ByteString -> IO ()) -> IO ())

runZmqRpc :: String
          -> ZmqHandler
          -> IO ()
runZmqRpc bind call = do
    withContext 1 (\c ->
        runZmqRpcWithContext c bind call)

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
            id <- receive s []
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
                _ <- forkIO $ call m (zmqRpcReply ctx inproc id)
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

serveMontageZmq :: (MontageRiakValue r) => r -> ConcurrentState -> LogCallback -> RiakPool -> Stats -> IO ()
serveMontageZmq b state log pool stats = do
    runZmqRpc "tcp://*:7078" wrapMontage
  where
    wrapMontage m cb = do
        case messageGet $ sTl m of
            Right (env, x) | B.length x == 0 -> do
                res <- try $ do
                    let !cmd = generateRequest b env
                    fmap (serializeResponse env) $ processRequest state log pool cmd stats
                case res of
                    Left (e :: SomeException) -> returnError (show e) $ msgid env
                    Right outenv -> cb $  messagePut outenv

            _ -> returnError "Failed to decode MontageEnvelope" Nothing
      where
        returnError msg msgid = do
            logError msg
            cb $ messagePut $ MontageEnvelope {
                  mtype = MONTAGE_ERROR
                , msg = messagePut $ MontageError (uFromString msg)
                , msgid = msgid
                }
