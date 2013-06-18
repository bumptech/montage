module Network.Riak.Montage.Protocol where

import System.Nitro
import System.UUID.V4 (uuid)
import Control.Monad (forever, void)
import Control.Concurrent (forkIO)
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
                                    serializeResponse, ConcurrentState(..))


type Handler = (S.ByteString -> (S.ByteString -> IO ()) -> IO ())

runNitroRpc :: String
            -> Handler
            -> IO ()
runNitroRpc binda call =
    withSocket (bind binda defaultOpts)
               (\s -> forever $ do
                   fr <- recv s []
                   m <- frameToBstr fr
                   void $ forkIO $ call m (nitroCallback s fr)
               )

nitroCallback :: NitroSocket
              -> NitroFrame
              -> S.ByteString
              -> IO ()
nitroCallback s fr out = do
    frBack <- bstrToFrame out
    reply s fr frBack []

serveMontage :: (MontageRiakValue r) =>
                   (MontageEnvelope -> ChainCommand r) ->
                   String -> ConcurrentState -> LogCallback ->
                   PoolChooser -> Stats -> Int -> Int -> Bool -> Bool -> IO ()
serveMontage generate runOn state logCB chooser' stats maxRequests' requestTimeout' readOnly' logCommands' = do
    nitroRuntimeStart
    runNitroRpc runOn wrapMontage
  where
    wrapMontage m cb = do
        case messageGet $ sTl m of
            Right (env, x) | B.length x == 0 -> do
                res <- try $ do
                    let !cmd = generate env
                    fmap (serializeResponse env) $ processRequest state chooser' cmd stats maxRequests' requestTimeout' readOnly' logCommands'
                case res of
                    Left (e :: SomeException) -> returnError (show e) $ msgid env
                    Right outenv -> cb . lTs . messagePut $ outenv

            _ -> returnError "Failed to decode MontageEnvelope" Nothing
      where
        returnError err msgid' = do
            logError err
            logCB "EXCEPTION" Nothing $ object ["error" .=  err ]
            cb . lTs . messagePut $ MontageEnvelope {
                  mtype = MONTAGE_ERROR
                , msg = messagePut $ MontageError (uFromString err)
                , msgid = msgid'
                }
