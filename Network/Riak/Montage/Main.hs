module Network.Riak.Montage.Main where

import Control.Monad (forever, void)
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Text as T

import Network.StatsWeb (initStats, addCounter, incCounter, runStats, Stats)

import Network.Riak.Montage.Protocol
import Network.Riak.Montage.Process (newEmptyConcurrentState)
import Network.Riak.Montage.Types
import Network.Riak.Montage.Util

montageStats :: [T.Text]
montageStats = [
    "pulse"
  , "requests"
  , "requests.slow"
  , "requests.many.siblings"
  , "requests.big"
  ]

sleepForever :: IO a
sleepForever = forever $ threadDelay (1000000 * 3600)

runDaemon :: (MontageRiakValue a) => LogCallback -> T.Text -> RiakPool -> a -> IO ()
runDaemon logger prefix chooser crap = do
    stats <- initStats prefix
    mapM_ (addCounter stats) montageStats

    state <- newEmptyConcurrentState

    void $ forkIO $ loggedSupervise logger "network-zeromq" $ serveMontageZmq crap state logger chooser stats
    void $ forkIO $ loggedSupervise logger "timekeeper" $ timeKeeper stats
    void $ forkIO $ runStats stats 3334
    sleepForever

timeKeeper :: Stats -> IO a
timeKeeper stats = forever $ do
    logError "TICK"
    incCounter "pulse" stats
    threadDelay (5 * 1000000)
