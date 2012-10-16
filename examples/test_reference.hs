{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}
import Data.Conduit.Pool (Pool, createPool)
import Network.Riak (defaultClient, connect, disconnect,
                    Client(port), Connection)

import qualified Data.Attoparsec.ByteString.Char8 as AttoC
import qualified Data.ByteString.Char8 as S

import Control.Monad
import Control.Exception (try, SomeException(..), fromException, throw)
import System.ZMQ as ZMQ
import System.IO
import Data.Word (Word8)

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as BW

import Text.ProtocolBuffers.WireMessage (messageGet, messagePut, Wire)
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)

import MontageClient
import Network.Riak.Montage.Proto.Montage.MontageObject as MO
import User.UserInfo as UI
import User.UserEvent as UE

-- utils

putDecimal' :: Int -> [Word8]
putDecimal' 0 = []
putDecimal' i = ((fromIntegral f) + 48) : putDecimal' (fromIntegral r)
  where
    (r, f) = i `divMod` 10
{-# INLINE putDecimal' #-}

putDecimal :: Int -> B.ByteString
putDecimal i | i < 0     = BW.pack $ (45 :: Word8) : (reverse $ putDecimal' $ abs i)
             | otherwise = BW.pack $ reverse $ putDecimal' i
{-# INLINE putDecimal #-}

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _) = error "fromRight got Left!"

getDecimal :: (Integral a) => S.ByteString -> a
getDecimal = fromRight . AttoC.parseOnly AttoC.decimal
{-# INLINE getDecimal #-}

-- test client

generateUIData :: (Int, Int) -> MontageObject
generateUIData (key, uid) = MontageObject Nothing "u-name" key' data' Nothing
  where
    key' = putDecimal $ fromIntegral key
    data' = messagePut $ UserInfo { uid = fromIntegral uid }

generateUEData :: (Int, Int) -> MontageObject
generateUEData (key, eid) = MontageObject Nothing "u-event" key' data' Nothing
  where
    key' = putDecimal $ fromIntegral key
    data' = messagePut $ UserEvent { eid = fromIntegral eid }

testGet :: MontagePool -> BW.ByteString -> BW.ByteString -> IO (Maybe MontageObject)
testGet pool bucket key = do
  mr <- try $ montageGet pool bucket key
  case mr of
      Left (e :: SomeException) -> do
          hPutStrLn stdout "This client fucked up"
          return Nothing
      Right results -> return results

testPut :: MontagePool -> MontageObject -> IO ()
testPut pool data' = do
  mr <- try $ montagePut pool data'
  case mr of
      Left (e :: SomeException) -> hPutStrLn stdout "This client fucked up"
      Right res -> do
        hPutStrLn stdout "\nput: "
        hPutStrLn stdout $ show res

testReferenceSet :: MontagePool -> BW.ByteString -> BW.ByteString -> BW.ByteString -> IO ()
testReferenceSet pool bucket key target = do
  mr <- try $ montageSetBy pool bucket key target
  case mr of
      Left (e :: SomeException) -> hPutStrLn stdout "This client fucked up"
      Right res -> do
          hPutStrLn stdout "\nset: "
          hPutStrLn stdout $ show res

testReferenceGet :: MontagePool -> IO ()
testReferenceGet pool = do
  let toKey = (putDecimal . fromIntegral)

  testPut pool $ generateUIData (1, 2) -- bucket=u-name, key=1, value=2
  testPut pool $ generateUEData (2, 3) -- bucket=u-event, key=2, value=3

  -- test reference get
  result <- montageGetBy pool "u-name" (toKey 1) "u-event"
  case result of
      Just res -> case dataBack res == (fromIntegral 3) of
          True -> hPutStrLn stdout $ "\nReference lookup correctly found: " ++ (show res)
          False -> hPutStrLn stdout $ "\nReference lookup failed: " ++ (show res)
        where dataBack = UE.eid . (messageGetError "u-event") . MO.data'
      Nothing -> hPutStrLn stdout $ "Found nothing from reference lookup"

threadCount :: Int
threadCount = 1

main :: IO ()
main = do
  ctx <- ZMQ.init 1

  -- Montage ZeroMQ
  montageZpool <- createPool (do
      s <- ZMQ.socket ctx Req
      ZMQ.connect s "tcp://localhost:7078"
      return s
      ) ZMQ.close 1 5 threadCount

  testReferenceGet montageZpool
