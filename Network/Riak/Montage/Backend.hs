module Network.Riak.Montage.Backend where

import Prelude hiding (catch)
import Control.Exception (finally, catch, throw, Exception(..), SomeException(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, readTVar, writeTVar, atomically)
import Control.Monad (mapM_, replicateM, when)
import Network.Riak (connect, disconnect, defaultClient, Client(..))
import Network.Riak.Types
import Network.Riak.Content (Content(..))
import Network.Riak.Value.Resolvable (getWithLength, put, delete)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import System.Random (getStdRandom, randomR)

import Network.StatsWeb (Stats, incCounter)

import Network.Riak.Montage.Types
import Data.Conduit.Pool (withResource)
import Data.Maybe (maybe)

import Unsafe.Coerce (unsafeCoerce)

maxRetries :: Int
maxRetries = 3

-- We can't actually put in the type signatures below, see
-- http://www.haskell.org/haskellwiki/Type_families#Injectivity.2C_type_inference.2C_and_ambiguity
retryOperation :: IO a -> IO a
retryOperation op =
    retryOperation' op 0
  where
    --retryOperation' :: IO a -> Int -> IO a
    retryOperation' op retries = catch op (handleError retries)

    --handleError :: (Exception e) => Int -> e -> IO a
    handleError retries e = case retries > maxRetries of
        True -> throw (e :: SomeException)
        False -> do
            threadDelay $ 100000 * retries
            retryOperation' op $ retries + 1

doGet :: (MontageRiakValue a) => Stats -> Bucket -> Key -> RiakPool -> IO (RiakResponse a)
doGet stats buck key pool = do
  res <- retryOperation $ withResource (pool buck) $ \c -> getWithLength c buck key Default
  case res of
    Just ((resolved, siblings), v) -> do
        --let resolvedLength = L.length $ riakSerialize resolved
        --when (siblings > 10) $ incCounter "requests.many.siblings" stats
        --when (resolvedLength > 1097152) $ incCounter "requests.big" stats
        return $ Just (resolved, v, Just siblings)
    Nothing -> return Nothing

doPut :: (MontageRiakValue r) => Bucket -> Key -> VectorClock -> RiakRecord r -> RiakPool -> IO (RiakResponse r)
doPut buck key mvc rec pool = do
  let riakvc = fmap VClock mvc
  res <- retryOperation $ withResource (pool buck) $ \c -> (fmap Just $ put c buck key riakvc (unsafeCoerce rec) (Just 1) Default Default)
  return $ fmap (\(r, v) -> (r, v, Nothing)) res

doDelete :: (MontageRiakValue a) => Bucket -> Key -> RiakPool -> IO (RiakResponse a)
doDelete buck key pool = do
  retryOperation $ withResource (pool buck) $ (\c -> delete c buck key Default >> return Nothing)
  return Nothing
