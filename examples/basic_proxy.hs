{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit.Pool (Pool, createPool)
import Network.Riak (defaultClient, connect, disconnect,
                    Client(port), Connection)

import qualified Data.ByteString.Lazy as BW
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson (Value, (.=), object, encode)
import Data.Word (Word8)

import Text.ProtocolBuffers.WireMessage (messageGet, messagePut, Wire)
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)
import Text.ProtocolBuffers.Basic (Utf8, toUtf8, uFromString)

import Network.Riak.Montage
import Network.Riak.Montage.Util

import User.UserInfo
import User.UserEvent

-- Logging

simpleCallback :: B.ByteString -> Maybe Double -> Value -> IO () -- :: LogCallback
simpleCallback logType _ val = logError $ (B.unpack logType) ++ " " ++ show val

-- Pool setup

generatePool :: String -> Int -> IO (Pool Connection)
generatePool port count =
    createPool
        (connect $ defaultClient {port = port})
        disconnect
        1  -- stripes
        10 -- timeout
        count -- max connections

chooseSinglePool :: Pool Connection -> RiakPool
chooseSinglePool pool = (\_ -> pool)

-- Resolution logic
{-
  Here your data can be one of two simple protocol buffers:
    message UserInfo {
      required uint32 uid = 1;
    }

    message UserEvent {
      required uint32 eid = 1;
    }
  .
  You always wrap it in a resolution data type so you can create a
  unique BucketSpec by bucket name.

  A simple last-write-wins resolution uses something like the
  lastWriteWins resolver (below). Don't forget to define a
    1. constructor (ByteString -> a) function
    2. deconstructor (a -> ByteString) function
  for each bucket
-}

data ResObject = ResObjectUserInfo UserInfo
               | ResObjectUserEvent UserEvent
  deriving (Show)

instance MontageRiakValue ResObject where
  getPB "u-name" = BucketSpec
                   PoolA
                   (ResObjectUserInfo . messageGetError "UserInfo")
                   lastWriteWins
                   undefined
                   (\(ResObjectUserInfo o) -> messagePut o)
  getPB "u-event" = BucketSpec
                   PoolA
                   (ResObjectUserEvent . messageGetError "UserEvent")
                   lastWriteWins
                   undefined
                   (\(ResObjectUserEvent o) -> messagePut o)
  getPB bucket = error $ B.unpack $ B.concat [ "No resolution function defined for bucket: ", bucket ]

  referenceKey (ResObjectUserInfo pb) = Just $ (putDecimal . fromIntegral) $ (uid pb)
  referenceKey (ResObjectUserEvent pb) = Just $ (putDecimal . fromIntegral) $ (eid pb)
  referenceKey _ = Nothing

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

lastWriteWins a b = b
lastWriteWinsBucketSpecA a b = BucketSpec PoolA a lastWriteWins undefined b

messageGetError :: (ReflectDescriptor a, Wire a) => B.ByteString -> B.ByteString -> a
messageGetError cls v =
    case messageGet v of
        Right (msg, x) | B.length x == 0 ->
            msg
        Right (_, x) | B.length x /= 0 ->
            error ("error decoding " ++ B.unpack cls ++ ": did not consume all of input")
        Left e ->
            error ("error decoding " ++ B.unpack cls ++ ": " ++ e)

-- Run it

main :: IO ()
main = do
  mainPool <- generatePool "8087" 300
  let chooser = chooseSinglePool mainPool

  let crap = (ResObjectUserInfo $ UserInfo { uid = fromIntegral 1 } ) :: ResObject
                                                           -- sets the type inference

  runDaemon simpleCallback "montage" chooser crap
