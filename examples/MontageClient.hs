{-# LANGUAGE OverloadedStrings #-}
module MontageClient where

import System.UUID.V4 (uuid)
import System.Timeout (timeout)
import System.ZMQ as ZMQ
import System.IO.Error (IOError(..), ioeGetErrorString)
import Control.Monad.Error (throwError, strMsg, Error, MonadError)
import Control.Exception (try, SomeException(..), fromException, throw)
import Control.Monad

import Data.Conduit.Pool (Pool, withResource)
import Data.Text.Format
import Data.Text.Format.Params
import qualified Data.Text.Lazy as T
import qualified Data.ListLike as LL
import qualified Data.Sequence as Seq

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as B

import Text.ProtocolBuffers.Basic (toUtf8, uToString)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut, Wire)
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)

import Network.Riak.Montage.Util
import Network.Riak.Montage.Proto.Montage.MontageWireMessages
import Network.Riak.Montage.Proto.Montage.MontageObject
import Network.Riak.Montage.Proto.Montage.MontageEnvelope as ME
import Network.Riak.Montage.Proto.Montage.MontageGet
import Network.Riak.Montage.Proto.Montage.MontageGetMany
import Network.Riak.Montage.Proto.Montage.MontageGetResponse as MGR
import Network.Riak.Montage.Proto.Montage.MontagePutResponse as MPR
import Network.Riak.Montage.Proto.Montage.MontagePutManyResponse as MPMR
import Network.Riak.Montage.Proto.Montage.MontagePutMany
import qualified Network.Riak.Montage.Proto.Montage.MontageError as MErr

type MontagePool = Pool (ZMQ.Socket Req)

-- utils

formatThrow :: (Params ps, Error e, MonadError e m, Monad m) => Format -> ps -> m ()
formatThrow f p = throwError (strMsg $ T.unpack $ format f p)

assertM :: (Params ps, Error e, MonadError e m, Monad m) => Bool -> Format -> ps -> m ()
assertM bool f p = when (not bool) $ formatThrow f p

messageGetError :: (ReflectDescriptor a, Wire a) => B.ByteString -> B.ByteString -> a
messageGetError cls v =
    case messageGet v of
        Right (msg, x) | B.length x == 0 ->
            msg
        Right (_, x) | B.length x /= 0 ->
            error ("error decoding " ++ B.unpack cls ++ ": did not consume all of input")
        Left e ->
            error ("error decoding " ++ B.unpack cls ++ ": " ++ e)

--- client

sixtySecs = 60 * 1000 * 1000

montageRpc :: MontagePool -> L.ByteString -> IO MontageEnvelope
montageRpc pool req = do
    mr <- timeout sixtySecs rpc
    case mr of
        Just r -> return $ (messageGetError "MontageEnvelope" . sTl) r
        Nothing -> error "timeout on montageRpc!"
  where
    rpc =   withResource pool (\socket -> do
                ZMQ.send' socket req []
                ZMQ.receive socket []
                )

montageGet :: MontagePool -> L.ByteString -> L.ByteString -> IO (Maybe MontageObject)
montageGet pool buck key = do
    uid <- fmap (B.pack . show) uuid
    let req = messagePut $ MontageEnvelope MONTAGE_GET (messagePut $ MontageGet buck key Nothing) (Just uid)
    res <- montageRpc pool req
    assertM (ME.msgid res == Just uid) "mismatched req/response!" ()
    case ME.mtype res of
        MONTAGE_GET_RESPONSE -> return $ MGR.master $ messageGetError "montageGet: MontageGetResponse" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontageGet: error={}, bucket={}, key={}" (Shown $ MErr.error msg, Shown buck, Shown key) >> return Nothing
          where msg = messageGetError "montageGet: MontageGetResponse" $ ME.msg res
        _                  -> formatThrow "Unknown response to MontageGet" () >> return Nothing

montagePutMany :: MontagePool -> [MontageObject] -> IO [MontageObject]
montagePutMany pool os = do
    uid <- fmap (B.pack . show) uuid
    let req = messagePut $ MontageEnvelope MONTAGE_PUT_MANY (messagePut $ MontagePutMany $ Seq.fromList os) (Just uid)
    res <- montageRpc pool req
    assertM (ME.msgid res == Just uid) "mismatched req/response!" ()
    case ME.mtype res of
        MONTAGE_PUT_MANY_RESPONSE -> return $ LL.map MPR.object $ MPMR.objects $ messageGetError "montagePutMany: put" $ ME.msg res
        MONTAGE_ERROR        -> formatThrow "Incorrect response to MontagePutMany: error={}" (Only $ uToString $ MErr.error msg) >> return []
          where msg = messageGetError "montagePutMany: MontageGetResponse" $ ME.msg res
        _ -> formatThrow "Unknown response to MontagePutMany" () >> return []
