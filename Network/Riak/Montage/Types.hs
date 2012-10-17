module Network.Riak.Montage.Types where

import Network.Riak.Montage.Proto.Montage.MontageWireMessages
import Text.ProtocolBuffers.WireMessage (Wire)
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)
import Network.Riak.Montage.Proto.Montage.MontageSubrequestSpec
import Network.Riak.Montage.Proto.Montage.MontageObject
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import Network.Riak.Types
import Data.Conduit.Pool (Pool)
import GHC.Int (Int64)

import Network.Riak (Resolvable(..))
import qualified Network.Riak.Value as V
import qualified Network.Riak.Content as C

class (Show a) => MontageRiakValue a where
    getPB :: L.ByteString -> BucketSpec a

    referenceKey :: a -> Maybe Key

    customCommandHandler :: T.Text -> Maybe L.ByteString -> ChainIteration a
    customCommandHandler cmd = error $ "No handler for custom command: " ++ T.unpack cmd

    riakSerialize :: RiakRecord a -> L.ByteString
    riakSerialize (RiakMontageLazyBs _ bs) = bs
    riakSerialize (RiakMontagePb b v) = (deconstruct $ getPB b) v

    ensureEval :: RiakRecord a -> RiakRecord a
    ensureEval (RiakMontageLazyBs b s) = RiakMontagePb b $ construct (getPB b) s
    ensureEval r@(RiakMontagePb _ _) = r
    ensureEval _ = error "non lazy or pb passed to pb `eval`"

instance (MontageRiakValue a) => Resolvable (RiakRecord a) where
    -- force deserialization for resolution
    resolve r1@(RiakMontageLazyBs _ _) r2 = resolve (ensureEval r1) r2
    resolve r1 r2@(RiakMontageLazyBs _ _) = resolve r1 (ensureEval r2)

    resolve (RiakMontagePb b o1) (RiakMontagePb _ o2) = RiakMontagePb b $ (pbResolve $ getPB b) o1 o2
    resolve _ _  = error "no resolution code for record type, wtfbbq?"

getReferenceKey :: (MontageRiakValue a) => RiakRecord a -> Maybe Key
getReferenceKey (RiakMontagePb _ v) = referenceKey v
getReferenceKey (RiakMontageLazyBs _ bstr) = Just bstr

instance (MontageRiakValue a) => V.IsContent (RiakRecord a) where
    parseContent buck c = return $ RiakMontageLazyBs buck $ C.value c
    toContent = C.binary . riakSerialize

showRiakRecord :: (Show a, Show b) => a -> b -> [Char]
showRiakRecord b v = "(buck=" ++ show b ++ ", val=" ++ show v ++ ")"

instance (MontageRiakValue a) => Show (RiakRecord a) where
    show (RiakMontageLazyBs b v) = showRiakRecord b v
    show (RiakMontagePb b v) = showRiakRecord b v

data PoolSpec = PoolA -- bam etc, ssd
              | PoolB -- boss, slower

type SubType = L.ByteString
type SubParam = Maybe L.ByteString

data (MontageRiakValue r) => BucketSpec r = BucketSpec {
          pool :: PoolSpec
        , construct :: Constructor r
        , pbResolve :: Resolver r
        , subrequest :: Subrequestor r
        , deconstruct :: Deconstructor r
        }

type Constructor a = L.ByteString -> a
type Resolver a = a -> a -> a
type Subrequestor a = a -> SubType -> SubParam -> [(Bucket, Key)]
type Deconstructor a = a -> L.ByteString

type VectorClock = Maybe L.ByteString

data (MontageRiakValue r) => RiakRecord r = RiakMontageLazyBs Bucket L.ByteString
                                          | RiakMontagePb Bucket r

data (MontageRiakValue r) => ChainIteration r =
      IterationRiakCommand [RiakRequest r] ([RiakResponse r] -> ChainCommand r)
    | IterationResponse CommandResponse
    | ChainIterationIO (IO (ChainCommand r))

data CommandResponse = forall a. (Wire a, ReflectDescriptor a) => ResponseProtobuf MontageWireMessages a
                     | ResponseCustom T.Text (Maybe L.ByteString)

data (MontageRiakValue r) => ChainCommand r =
      ChainGet Bucket Key (Maybe MontageSubrequestSpec) (Maybe ([RiakResponse r] -> ChainCommand r))
    | ChainGetMany [(Bucket, Key)] (Maybe MontageObject) (Maybe ([RiakResponse r] -> ChainCommand r))
    | ChainPut VectorClock Bucket Key (RiakRecord r) (Maybe ([RiakResponse r] -> ChainCommand r))
    | ChainPutMany [(VectorClock, Bucket, Key, RiakRecord r)] (Maybe ([RiakResponse r] -> ChainCommand r))
    | ChainDelete Bucket Key (Maybe ([RiakResponse r] -> ChainCommand r))
    | ChainReference Bucket Key Bucket (Maybe MontageSubrequestSpec)
    | ChainReferenceSet Bucket Key Key
    | ChainSub (RiakRecord r) Specifier Argument
    | ChainCustom T.Text (Maybe L.ByteString)
    | ChainReturn CommandResponse
    | ChainCommandIO (IO (ChainCommand r))

data (MontageRiakValue a) => RiakRequest a = RiakGet Bucket Key
                                           | RiakPut VectorClock Bucket Key (RiakRecord a)
                                           | RiakDelete Bucket Key

type RiakResponse a = Maybe (RiakRecord a, VClock, Maybe Int)

type RiakPool = Bucket -> Pool Connection

type RawValue = S.ByteString
type Specifier = T.Text
type Argument = S.ByteString
