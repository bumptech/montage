{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}
import Data.Conduit.Pool (Pool, createPool)
import Network.Riak (defaultClient, connect, disconnect,
                    Client(port), Connection)

import qualified Data.Attoparsec.ByteString.Char8 as AttoC
import qualified Data.ByteString.Char8 as S

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

generateData :: (Int, Int) -> MontageObject
generateData (key, uid) = MontageObject Nothing "u-name" key' data' Nothing
  where
    key' = putDecimal $ fromIntegral key
    data' = messagePut $ UserInfo { uid = fromIntegral uid
                                  , name = Nothing }

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

testLastWriteWins :: MontagePool -> BW.ByteString -> Int -> IO ()
testLastWriteWins pool bucket key = do
  let putter = (\uid -> testPut pool $ generateData (key, fromIntegral uid))
  mapM_ putter [1..4]
  res <- testGet pool bucket (putDecimal key)
  case res of
      Just r -> case dataBack r == (fromIntegral 4) of
          True -> hPutStrLn stdout $ "\nLast Write Won: " ++ (show r)
          False -> hPutStrLn stdout $ "\nLast Write Lost: " ++ (show r)
        where dataBack = UI.uid . (messageGetError "u-name") . MO.data'
      Nothing -> hPutStrLn stdout $ "\nPuts failed entired, nothing in bucket: " ++ (show bucket) ++ ", key: " ++ (show key)

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

  testLastWriteWins montageZpool bucket key
