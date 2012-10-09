module Network.Riak.Montage.Types where

import Network.Riak.Montage.Proto.Montage.MontageWireMessages
import Text.ProtocolBuffers.WireMessage (Wire)
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)
import Network.Riak.Montage.Proto.Montage.MontageSubrequestSpec
import Network.Riak.Montage.Proto.Montage.MontageObject
import Data.Word (Word32)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe, Maybe)
import Data.Aeson.Types (Result(..))
import qualified Data.Text as T
import Network.Riak.Types
import Network.Riak (Client(..))
import Data.Conduit.Pool (Pool)
import Control.Monad.Reader (ReaderT)
import qualified Data.Map as M
import Control.Concurrent.STM (TVar(..))

import Network.Riak (Resolvable(..))
import qualified Network.Riak.Value as V
import qualified Network.Riak.Content as C

class (Show a) => MontageRiakValue a where
    getPB :: L.ByteString -> BucketSpec a

    customCommandHandler :: T.Text -> Maybe L.ByteString -> ChainIteration a
    customCommandHandler cmd = error $ "No handler for custom command: " ++ T.unpack cmd

    riakSerialize :: RiakRecord a -> L.ByteString
    riakSerialize (RiakMontageReference _ bs) = bs
    riakSerialize (RiakMontageLazyBs _ bs) = bs
    riakSerialize (RiakMontagePb b v) = (deconstruct $ getPB b) v

    ensureEval :: RiakRecord a -> RiakRecord a
    ensureEval (RiakMontageLazyBs b s) = RiakMontagePb b $ construct (getPB b) s
    ensureEval r@(RiakMontagePb _ _) = r
    ensureEval _ = error "non lazy or pb passed to pb `eval`"

instance (MontageRiakValue a) => Resolvable (RiakRecord a) where
    resolve r1@(RiakMontageReference _ _) r2@(RiakMontageReference _ _) = r1

    -- force deserialization for resolution
    resolve r1@(RiakMontageLazyBs b s1) r2 =
        resolve (ensureEval r1) r2
    resolve r1 r2@(RiakMontageLazyBs b s2) =
        resolve r1 (ensureEval r2)

    resolve r1@(RiakMontagePb b o1) r2@(RiakMontagePb _ o2) = RiakMontagePb b $ (pbResolve $ getPB b) o1 o2
    resolve r1 r2 = error "no resolution code for record type, wtfbbq?"

refPfx = "reference-"
rlen = L.length refPfx

instance (MontageRiakValue a) => V.IsContent (RiakRecord a) where
    parseContent bucket c | L.take rlen bucket == refPfx = return $ RiakMontageReference bucket $ C.value c
                          | otherwise                    = return $ RiakMontageLazyBs bucket $ C.value c

    toContent = C.binary . riakSerialize

instance (MontageRiakValue a) => Show (RiakRecord a) where
    show = undefined

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
                                    | RiakMontageReference Bucket L.ByteString

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
