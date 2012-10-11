module Network.Riak.Montage.Util where

import Control.Monad (forever)
import Control.Exception (try, SomeException)
import Control.Applicative
import Control.Concurrent (threadDelay)

import Data.Aeson (Value, (.=), object)

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as S


type LogCallback = B.ByteString -> Maybe Double -> Value -> IO ()

logError :: String -> IO ()
logError s = do
    time <- formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" <$> getCurrentTime
    hPutStrLn stderr $ "[" ++ time ++ "] " ++ s

sTl :: S.ByteString -> B.ByteString
sTl s = B.fromChunks [s]

lTs :: B.ByteString -> S.ByteString
lTs l = S.concat $ B.toChunks l

supervise :: String -> IO () -> IO ()
supervise label a = loggedSupervise noopLog label a
    where
        noopLog _ _ _ = threadDelay 200000

loggedSupervise :: LogCallback -> String -> IO () -> IO ()
loggedSupervise netlog label a = forever $ do
    logStart
    res <- try a
    case res of
        Left (e :: SomeException) -> logException e
        Right _               -> logTerminate -- unexpected!
  where
    logStart = logError ("{supervisor " ++ label ++ "} RESTARTED")
    logException e = do
        logError ("{supervisor " ++ label ++ "} EXCEPTION: "++ (show e))
        netlog "EXCEPTION" Nothing $ object ["label" .= label,
                                             "exception" .= show e]
    logTerminate = logError ("{supervisor " ++ label ++ "} EXITED!?")
