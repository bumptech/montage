{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit.Pool (Pool, createPool)
import Network.Riak (defaultClient, connect, disconnect,
                    Client(port), Connection)

import qualified Data.Attoparsec.ByteString.Char8 as AttoC
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as S
import Data.Aeson (Value, (.=), object, encode)
import Data.Char
import Data.List
import Data.Bits

import Text.ProtocolBuffers.WireMessage (messageGet, messagePut, Wire)
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)
import Text.ProtocolBuffers.Basic (Utf8, toUtf8, uFromString)

import Network.Riak.Montage
import Network.Riak.Montage.Util

import User.UserInfo

-- Logging

sillyCallback :: B.ByteString -> Maybe Double -> Value -> IO () -- :: LogCallback
sillyCallback logType _ val = logError $ (B.unpack logType) ++ " " ++ show val

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
  Here your data is not just a protocol buffer, but one of several types:
    a protocol buffer
    a single String
      where the first 4 characters are an integer value that
      encodes the size of a record and the rest of the string
      encodes other record properties.

  This example shows a more complicated resolution than simply last write
  wins.

-}

data ResObject = ResObjectUserInfo UserInfo
               | ResObjectRecord B.ByteString
  deriving (Show)

instance MontageRiakValue ResObject where
  getPB "u-name" = BucketSpec
                   PoolA
                   (ResObjectUserInfo . messageGetError "u-name")
                   lastWriteWins
                   undefined
                   (\(ResObjectUserInfo o) -> messagePut o)
  getPB "fk-pair" = BucketSpec
                    PoolA
                    (ResObjectRecord . id)
                    resolve
                    undefined
                    (\(ResObjectRecord o) ->  o)
    where
      resolve (ResObjectRecord o1) (ResObjectRecord o2) =
          ResObjectRecord $ takeLargerRecord o1 o2
        where
          takeLargerRecord o1 o2 | size o1 >= size o2 = o1
                                 | otherwise = o2
          size :: B.ByteString -> Int
          size = getDecimal . lTs . (B.take 4)

  getPB bucket = error $ B.unpack $ B.concat [ "No resolution function defined for bucket: ", bucket ]

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _) = error "fromRight got Left!"

getDecimal :: (Integral a) => S.ByteString -> a
getDecimal = fromRight . AttoC.parseOnly AttoC.decimal
{-# INLINE getDecimal #-}

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
  let logCallback = sillyCallback

  mainPool <- generatePool "8087" 300
  let chooser = chooseSinglePool mainPool

  let crap = (ResObjectUserInfo $ UserInfo { uid = fromIntegral 1
                                           , name = Nothing } ) :: ResObject
                                                           -- sets the type inference

  runDaemon logCallback "montage" chooser crap
