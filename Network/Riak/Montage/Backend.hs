module Network.Riak.Montage.Backend where

import Prelude hiding (catch)
import Control.Monad (void, when)
import Control.Exception (catch, throw, SomeException(..))
import Control.Concurrent (threadDelay)
import Data.ByteString.Lazy as L
import Network.Riak.Types
import Network.Riak.Value.Resolvable (getWithLengthOpt, put, delete)

import Network.StatsWeb (Stats, incCounter)

import Network.Riak.Montage.Types
import Data.Conduit.Pool (withResource)

maxRetries :: Int
maxRetries = 3

-- We can't actually put in the type signatures below, see
-- http://www.haskell.org/haskellwiki/Type_families#Injectivity.2C_type_inference.2C_and_ambiguity
retryOperation :: IO a -> IO a
retryOperation op =
    retryOperation' 0
  where
    --retryOperation' :: IO a -> Int -> IO a
    retryOperation' retries = catch op (handleError retries)

    --handleError :: (Exception e) => Int -> e -> IO a
    handleError retries e = case retries > maxRetries of
        True -> throw (e :: SomeException)
        False -> do
            threadDelay $ 100000 * retries
            retryOperation' $ retries + 1

doGet :: (MontageRiakValue r) => Stats -> Bucket -> Key -> PoolChooser -> BucketOpts r -> IO (RiakResponse r)
doGet stats buck key chooser' opts' = do
  res <- retryOperation $ withResource (chooser' buck) $ \c -> getWithLengthOpt c buck key Default (basic_quorum opts') (notfound_ok opts')
  case res of
    Just ((resolved, siblings), v) -> do
        let resolvedLength = L.length $ riakSerialize resolved
        when (siblings > 10) $ incCounter "requests.many.siblings" stats
        when (resolvedLength > 1097152) $ incCounter "requests.big" stats
        return $ Just (resolved, v, Just siblings)
    Nothing -> return Nothing


doPut :: (MontageRiakValue r) => Bucket -> Key -> VectorClock -> RiakRecord r -> PoolChooser -> IO (RiakResponse r)
doPut buck key mvc rec chooser' = do
  let riakvc = fmap VClock mvc
  res <- retryOperation $ withResource (chooser' buck) $ \c -> (fmap Just $ put c buck key riakvc rec (Just 1) Default Default)
  return $ fmap (\(r, v) -> (r, v, Nothing)) res

doDelete :: (MontageRiakValue a) => Bucket -> Key -> PoolChooser -> IO (RiakResponse a)
doDelete buck key chooser' = do
  void $ retryOperation $ withResource (chooser' buck) $ (\c -> delete c buck key Default >> return Nothing)
  return Nothing
