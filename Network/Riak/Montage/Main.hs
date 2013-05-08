module Network.Riak.Montage.Main where

import System.IO (hSetBuffering, BufferMode(..), stdout, stderr)
import Control.Monad (forever, void)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent (newEmptyMVar)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B

import Network.StatsWeb (initStats, addCounter, incCounter, runStats, Stats)

import Data.Pool (Pool, createPool')
import Network.Riak (defaultClient, connect, disconnect,
                    Client(port), Connection)

import Network.Riak.Montage.Protocol
import Network.Riak.Montage.Process (generateRequest)
import Network.Riak.Montage.Types
import Network.Riak.Montage.Util


{- Stats broadcast to localhost, port 3344
"pulse"
"requests"
"requests.slow"
"requests.many.siblings"
"requests.big"
"requests.siblings[bucket={}]" (bucket from put)
"requests.custom[type={}]" (custom command handler Text)
-}

sleepForever :: IO a
sleepForever = forever $ threadDelay (1000000 * 3600)

simpleCallback :: LogCallback
simpleCallback logType _ val = logError $ (B.unpack logType) ++ " " ++ show val

-- | Proxy configuration.  Configurable fields: proxyPort, logger, statsPrefix.  Non-configurable fields: generator.
cfg :: (MontageRiakValue a) => Config a
cfg = Config {
     proxyPort = 7078
   , logger = simpleCallback
   , statsPrefix = "montage"
   , statsPort = 3334
   , generator = generateRequest
   , maxRequests = 700
   , requestTimeout = 30
   , readOnly = False
   , logCommands = False
  }

-- | Create a pool of Riak connection (usually used for constructing the second argument of @runDaemon@), given a port and a max number of connections.
riakPoolOnPort :: String -> Int -> IO (Pool Connection)
riakPoolOnPort port' count = riakPoolOnPort' port' count 0

-- | Use tracking as the number of seconds delay before calculating pool resource and stm stats.  Stats written to stderr.
riakPoolOnPort' :: String -> Int -> Int -> IO (Pool Connection)
riakPoolOnPort' port' count tracking =
    createPool'
        (connect $ defaultClient {port = port'})
        disconnect
        1  -- stripes
        10 -- timeout
        tracking -- tracking turned on if non zero
        count -- max connections

-- | Start the resolution proxy, where you define resolutions for your data @a@, and create one or more Riak connection pools @p@.
runDaemon :: (MontageRiakValue a, Poolable p) => Config a -> p -> IO ()
runDaemon cfg' pools = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    stats <- initStats (statsPrefix cfg')

    q <- newEmptyMVar

    let chooser' = chooser pools
    let logging = logger cfg'
    let runOn = "tcp://*:" ++ show (proxyPort cfg')

    let loop = processLoop q (generator cfg') logging chooser' stats (requestTimeout cfg') (readOnly cfg') (logCommands cfg')
    mapM_ (makeChild loop) [0..15] -- 15 threads temp

    void $ forkIO $ loggedSupervise logging "network-zeromq" $ serveMontageZmq q runOn
    void $ forkIO $ loggedSupervise logging "timekeeper" $ timeKeeper stats
    void $ forkIO $ runStats stats (statsPort cfg')
    sleepForever

makeChild :: IO () -> Int -> IO ()
makeChild loop n = void $ forkIO $ loggedSupervise log label $ loop
    where
        label = "node-" ++ show n

timeKeeper :: Stats -> IO a
timeKeeper stats = forever $ do
    logError "TICK"
    incCounter "pulse" stats
    threadDelay (5 * 1000000)
