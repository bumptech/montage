{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}
import Data.Conduit.Pool (Pool, createPool)
import Network.Riak (defaultClient, connect, disconnect,
                    Client(port), Connection)

import Control.Exception (try, SomeException(..), fromException, throw)
import System.ZMQ as ZMQ
import System.IO
import Data.Word (Word8)

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as BW

import Text.ProtocolBuffers.WireMessage (messageGet, messagePut, Wire)
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)

import MontageClient
import Network.Riak.Montage.Proto.Montage.MontageObject
import User.UserInfo

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

-- test client

generateData :: (Int, Int) -> MontageObject
generateData (key, uid) = MontageObject Nothing "u-name" key' data' Nothing
  where
    key' = putDecimal $ fromIntegral key
    data' = messagePut $ UserInfo { uid = fromIntegral uid }

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

  let (bucket, key) = ("u-name", 1) :: (BW.ByteString, Int)
  let data' = map generateData $ zip [key | x <- [0..2]] [1..3]

  -- test put
  mr <- try $ montagePutMany montageZpool data'
  case mr of
      Left (e :: SomeException) -> hPutStrLn stdout "This client fucked up"
      Right results -> do
        hPutStrLn stdout "\nput: "
        case results of
            Just res -> mapM_ (\r -> hPutStrLn stdout $ show r) res
            Nothing -> hPutStrLn stdout "Nothing put in riak"

  -- test get and resolution
  mr <- try $ montageGet montageZpool bucket (putDecimal key)
  case mr of
      Left (e :: SomeException) -> hPutStrLn stdout "This client fucked up"
      Right results -> case results of
          Just mo -> do
              hPutStrLn stdout "\ngot: "
              hPutStrLn stdout $ show mo
          Nothing -> hPutStrLn stdout $ "no value at bucket: " ++ (show bucket) ++ ", key: " ++ (show key)
