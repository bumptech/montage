========================
Montage: a riak resolution proxy
========================

Even Rocky had a montage...

Install
=======

Built and tested with ghc 7.4.1

Install the non-hackage dependencies::

    git clone git@github.com:wmoss/StatsWeb.git
    cd StatsWeb && cabal install

    git clone git@github.com:bumptech/riak-haskell-client.git
    cd riak-haskell-client && cabal install

From montage/ execute::

    cabal install

We recommend using a sandbox, hsenv is particularly good.

Configuration
=======

The montage proxy is port configurable (here given the Config handle set to 7078), with riak running on 8087::

    import Network.Riak (defaultClient, connect, disconnect, Client(port), Connection)
    import Data.Conduit.Pool (Pool, createPool)

    import Network.Riak.Montage

    main :: IO ()
    main = do
        mainPool <- createPool
	    (connect $ defaultClient {port = "8087"})
	    disconnect
	    1 -- stripes
	    10 -- timeout
	    300 -- max connections

	let cfg' = cfg { proxyPort = 7078 }

	runDaemon (cfg' :: Config ResObject) mainPool

ResObject is a concrete datatype you define.  For Montage to work with this datatype, you must define:
  (1) translations from bytestring -> datatype, datatype -> bytestring,
  (2) methods for resolving siblings,
  (3) and how to obtain a bytestring key from values in the datatype
See examples/ for a definition of ResObject and how to implement (1), (2), and (3).

You can also set the mainPool argument to be a datatype with more than one pool, in case particular buckets have such variable resolution times that you want to isolate your bucket fetches::

    data Pools = Pools {
        poolA :: Pool Connection
      , poolB :: Pool Connection
    }

    instance Poolable Pools where
        chooser pools "bucketA" = poolA pools
        chooser pools "bucketB" = poolB pools
        chooser pools "bucketC" = poolC pools

For a single pool, like mainPool, you're also required to define its datatype as an instance of Poolable, such that the bucket doesn't matter::

    instance Poolable (Pool Connection) where
        chooser pool _ = pool

You can also a set the cfg field 'logger' to a logging function that fits the type signature::

    logger :: B.Bytestring -> Maybe Double -> Value -> IO ()
    logger errorType duration val = -- your defintion

where errorType is filled in by the Montage exception handler with the type of exception (useful as a prefix), duration is a timestamp Montage may provide (depending on where the error occurs), and val is JSON that Montage constructs with details about the error.

Since Montage provides these values, your logger would simply format the arguments and broadcasts them to a location of your choice.  The default logger simply concatenates the arguments and broadcasts to stderr.

Examples
=======
To setup the examples, first download hprotoc::

    cabal install hprotoc

Then execute::

    cd examples && hprotoc user.proto

Then::

    cd examples && runhaskell basic_proxy.hs

For client examples to run against the basic_proxy, see github.com/bumptech/montage-haskell-client/examples

More
===========

See the haddock documentation for type-signatures, descriptions, and source of client functions.
