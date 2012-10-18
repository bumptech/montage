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

import Text.ProtocolBuffers.Basic (toUtf8)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut, Wire)
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)

import MontageClient
import Network.Riak.Montage.Proto.Montage.MontageObject as MO

import User.UserInfo as UI
import User.UserEvent as UE
import User.UserName as UN

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
generateUIData (key, uid) = MontageObject Nothing "u-info" key' data' Nothing
  where
    key' = putDecimal $ fromIntegral key
    data' = messagePut $ UserInfo { uid = fromIntegral uid }

getUI :: MontageObject -> UserInfo
getUI = (messageGetError "u-info") . MO.data'

generateUEData :: (Int, Int) -> MontageObject
generateUEData (key, eid) = MontageObject Nothing "u-event" key' data' Nothing
  where
    key' = putDecimal $ fromIntegral key
    data' = messagePut $ UserEvent { eid = fromIntegral eid }

getUE :: MontageObject -> UserEvent
getUE = (messageGetError "u-event") . MO.data'

generateUNData :: (Int, BW.ByteString) -> MontageObject
generateUNData (key, nameStr) = MontageObject Nothing "u-name" key' data' Nothing
  where
    key' = putDecimal $ fromIntegral key
    data' = messagePut $ UserName { name = nameStr }

getUN :: MontageObject -> UserName
getUN = (messageGetError "u-name") . MO.data'

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

testReferenceGet :: MontagePool -> IO ()
testReferenceGet pool = do
    let toKey = (putDecimal . fromIntegral)

    testPut pool $ generateUIData (1, 2) -- bucket=u-name, key=1, value=2
    testPut pool $ generateUEData (2, 3) -- bucket=u-event, key=2, value=3
    testPut pool $ generateUNData (2, "montage") -- bucket=u-name, key=2, value="montage"

    -- test reference get on multiple targets: "u-event" "u-name"
    (subKey, valuesFound) <- montageGetBy pool "u-info" (toKey 1) ["u-event", "u-name"]
    case subKey of
        Just key ->
            if UI.uid (getUI key) == (fromIntegral 2) then successK key else failureK key
        Nothing -> hPutStrLn stdout "\nReference get couldn't find subKey"

    case length valuesFound == 2 of
        True -> do
            let [ue, un] = valuesFound
            if UE.eid (getUE ue) == (fromIntegral 3) then successV ue else failureV ue
            if UN.name (getUN un) == "montage" then successV un else failureV un
        False -> hPutStrLn stdout $ "\nReference get couldn't find the correct number of values. Looking for 2, found " ++ (show $ length valuesFound)
  where
    successK k = hPutStrLn stdout $ "\nReference get found correct key: " ++ show k
    failureK k = hPutStrLn stdout $ "\nReference get found incorrect key: " ++ show k

    successV v = hPutStrLn stdout $ "\nReference get found correct value: " ++ show v
    failureV v = hPutStrLn stdout $ "\nReference get found incorrect value: " ++ show v

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
