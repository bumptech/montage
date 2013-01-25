module Network.Riak.Montage
       (
        -- * Basic proxy
          runDaemon
        , cfg

        -- ** Resolutions
        -- $montageRiakValue

        -- ** Riak connection pools
        -- $pools

        -- ** Configuration
        -- $config

        -- * Proxy types
        , MontageRiakValue(..)
        , Poolable(..)
        , BucketSpec(..)
        , BucketOpts(..)
        , defaultBucketOpts
        , ChainCommand(..)
        , RiakRequest(..)
        , RiakRecord(..)
        , CommandResponse(..)
        , ChainIteration(..)
        , RiakResponse
        , evalRiakResponse
        , PoolChooser
        , Bucket
        , VClock
        , fromVClock
        , Config(..)
        , LogCallback
        , riakPoolOnPort
        , riakPoolOnPort'
        , Connection
        )
       where

import Network.Riak.Montage.Main
import Network.Riak.Montage.Types
import Network.Riak.Montage.Util
import Network.Riak.Types (Bucket, VClock, fromVClock)
import Network.Riak (Connection)

-- $montageRiakValue
-- You define your resolutions by saying that your concrete type, ResObject here, is a instance of the 'MontageRiakValue' typeclass:
--
-- @data ResObject = ResObjectConstructorA YourProtobuf
--               | ResObjectConstructorB SomeOtherType
--
--instance 'MontageRiakValue' ResObject where
--    getPB \"bucketA\" = 'BucketSpec'
--                      (ResObjectConstructorA . protobufDeserializer) /-- deserializer/
--                      (\a b -> b) /-- resolution function, here last write wins/
--                      (\(ResObjectConstructorA pb) -> protobufSerializer pb) /-- serializer/
--    getPB \"bucketB\" = 'BucketSpec'
--                     (ResObjectConstructorB . someOtherTypeDeserializer)
--                     (\a b -> a) /-- first write wins/
--                     (\(ResObjectConstructorB x) -> someOtherTypeSerializer x)
--    referenceKey (ResObjectConstructorA pb) = 'Just' (toByteString . intField) $ pb
--    referenceKey (ResObjectConstructorB x) = 'Nothing' /-- no reference key for this type/
-- @
--
-- The getPB function defines how to deserialize your data (as it comes from requests, or from Riak itself), how to resolve siblings if found, and how to serialize your data (to give it back in a response, or to put to Riak).  The reference Key function can define how to make a key from your data, in case you want to chain get requests together (one bucket-key's value is the key to another bucket, etc).
--

-- $pools
-- Montage is setup to allow you to configure buckets to pools, in case you want to process all of your fast fetches on one pool and a few slow fetches on another.  Montage defines the 'Poolable' typeclass for this. For a single pool, where buckets are all handled the same:
--
-- @instance 'Poolable' ('Pool' 'Connection') where
--    chooser p _ = p
-- @
--
-- For two pools:
--
-- @data Pools = Pools {
--    poolA :: 'Pool' 'Connection'
--    poolB :: 'Pool' 'Connection'
-- }
--
-- instance 'Poolable' Pools where
--    chooser p \"bucketA\" = poolA p
--    chooser p \"bucketB\" = poolB p
-- @
--

-- $config
-- To start the proxy, give @runDaemon@ the premade configuration @cfg@, cast to the concrete datatype for which you defined resolutions.  This premade configuration contains a proxyPort which you can modify (or use the default 7078), a 'LogCallback', a stats prefix, and a /non-configurable/ request generator that is the first point of entry into the type inference.
--
-- @import Data.Conduit.Pool (Pool, createPool)
--import Network.Riak (defaultClient, connect, disconnect,
--                    Client(port), Connection)
--
--main :: IO ()
--main = do
--    mainPool <- createPool
--                    (connect $ defaultClient { port = \"8087\" })
--                    disconnect
--                    1 /-- stripes/
--                    10 /-- timeout/
--                    300 /-- max connections/
--    let cfg' = cfg { proxyPort = 7777
--                   , statsPrefix = \"montage.requests.\"}
--    runDaemon (cfg' :: 'Config' ResObject) mainPool
-- @
--
-- See <https://github.com/bumptech/montage/tree/master/examples> for a runnable basic proxy.  See <https://github.com/bumptech/montage-haskell-client/tree/master/examples> for a client and sample requests to run against it.
