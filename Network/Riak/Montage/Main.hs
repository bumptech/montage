module Network.Riak.Montage.Main where

import Control.Exception (finally)
import Control.Monad (forever, liftM, void)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTVarIO, atomically, readTVar)
import System.IO (hSetBuffering, BufferMode(..), stdout, stderr)
import System.Environment (getEnvironment)
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T

import Data.Conduit.Pool (Pool, createPool)
import Network.Riak (defaultClient, connect, disconnect,
                    Client(port), Connection)
import Network.Riak.Types (Bucket)

import Network.StatsWeb (initStats, addCounter, incCounter, runStats)

import Network.Riak.Montage.Protocol
import Network.Riak.Montage.Process (ConcurrentState(..))
import Network.Riak.Montage.Types
import Network.Riak.Montage.Util

magicStats = [
    "pulse"
  , "requests"
  , "requests.slow"
  , "requests.many.siblings"
  , "requests.big"
  ]

sleepForever = forever $ threadDelay (1000000 * 3600)

runDaemon :: (MontageRiakValue a) => LogCallback -> T.Text -> RiakPool -> a -> IO ()
runDaemon log prefix chooser crap = do
    stats <- initStats prefix
    mapM_ (addCounter stats) magicStats

    count <- newTVarIO 0
    tick <- newTVarIO 0
    ts <- newTVarIO 0.0
    pipeline <- newTVarIO HM.empty
    let state = ConcurrentState {concurrentCount = count,
                                tick = tick,
                                ts = ts,
                                pipeline = pipeline}

    void $ forkIO $ loggedSupervise log "network-zeromq" $ serveMontageZmq crap state log chooser stats
    void $ forkIO $ loggedSupervise log "timekeeper" $ timeKeeper stats
    void $ forkIO $ runStats stats 3334
    sleepForever

timeKeeper stats = forever $ do
    logError "TICK"
    incCounter "pulse" stats
    threadDelay (5 * 1000000)
