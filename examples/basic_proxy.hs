{-# LANGUAGE OverloadedStrings, FlexibleInstances  #-}
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

import User.UserInfo as UI
import User.UserEvent as UE
import User.UserName as UN

-- Pool setup

generatePool :: String -> Int -> IO (Pool Connection)
generatePool port count =
    createPool
        (connect $ defaultClient {port = port})
        disconnect
        1  -- stripes
        10 -- timeout
        count -- max connections

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
               | ResObjectUserName UserName
  deriving (Show)

instance Poolable (Pool Connection) where
  chooser p _ = p

instance MontageRiakValue ResObject where
  getPB "u-info" = BucketSpec
                   (ResObjectUserInfo . messageGetError "UserInfo")
                   lastWriteWins
                   (\(ResObjectUserInfo o) -> messagePut o)
  getPB "u-event" = BucketSpec
                   (ResObjectUserEvent . messageGetError "UserEvent")
                   lastWriteWins
                   (\(ResObjectUserEvent o) -> messagePut o)
  getPB "u-name" = BucketSpec
                   (ResObjectUserName . messageGetError "UserName")
                   lastWriteWins
                   (\(ResObjectUserName o) -> messagePut o)
  getPB bucket = error $ B.unpack $ B.concat [ "No resolution function defined for bucket: ", bucket ]

  referenceKey (ResObjectUserInfo pb) = Just $ (putDecimal . fromIntegral . UI.uid) $ pb
  referenceKey (ResObjectUserEvent pb) = Just $ (putDecimal . fromIntegral . UE.eid) $ pb
  referenceKey (ResObjectUserName pb) = Nothing -- no reference key for this string value
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
  let cfg' = cfg { proxyPort = 7078 }
             -- how to set the port
             -- already default to 7078, so this does nothing
  runDaemon (cfg' :: Config ResObject) mainPool
