module Network.Riak.Montage.Commands where

import Network.Riak.Montage.Proto.Montage.MontageWireMessages
import Text.ProtocolBuffers.WireMessage (Wire, messagePut, messageGet)
import Text.ProtocolBuffers.Basic (toUtf8, utf8)
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)
import Data.Word (Word32)
import Data.List (foldl', nub)
import Data.Maybe (fromJust, fromMaybe, isNothing, isJust, catMaybes)
import qualified Data.Set as Set
import qualified Network.Riak.Content as C
import qualified Network.Riak.Types as RT
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import Data.Sequence ((<|))
import Data.Function (on)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ListLike as LL
import qualified Data.Aeson as AE
import Data.Aeson (Value, encode, (.=))
import Data.Bits (shiftL, (.|.))
import Data.Binary.Put (runPut, putWord64le)
import GHC.Word (Word32, Word64)
import Text.Printf (printf)

import Control.Arrow ((&&&))
import Control.Monad (forM, forM_, when)
import Control.Monad.Reader (asks)
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Hex (unhex, hex)
import System.IO.Unsafe (unsafePerformIO)
import Safe (fromJustNote)
import Data.Char (ord, toLower)

import Network.Riak.Types

import Network.Riak.Montage.Proto.Montage.MontageObject
import Network.Riak.Montage.Proto.Montage.MontageGetResponse
import Network.Riak.Montage.Proto.Montage.MontagePutResponse
import Network.Riak.Montage.Proto.Montage.MontagePutManyResponse
import Network.Riak.Montage.Proto.Montage.MontageGetStatus
import Network.Riak.Montage.Proto.Montage.MontageSubrequestSpec as MSS
import Network.Riak.Montage.Proto.Montage.MontageDeleteResponse

import Network.Riak.Montage.Types
import Network.Riak.Montage.Backend (doGet, doPut)

import Debug.Trace (trace)

makeObject :: (MontageRiakValue r) => L.ByteString -> L.ByteString -> (RiakRecord r, VClock, Maybe Int) -> MontageObject
makeObject buck key (rec, vclock, resolutions) =
    MontageObject
        (Just $ RT.fromVClock $ vclock)
        buck
        key
        (riakSerialize rec)
        (fmap fromIntegral resolutions)



exec :: ( MontageRiakValue r) => ChainCommand r -> ChainIteration r
exec (ChainGet buck key msub mfollow) =
    IterationRiakCommand [RiakGet buck key] $ fromMaybe finishGet mfollow
  where
    finishGet :: (MontageRiakValue r) => [RiakResponse r] -> ChainCommand r
    finishGet (mres:[]) =
        case mres of
            Just resp@(val, _, _) -> let obj = (Just $ makeObject buck key resp) in
                         case msub of
                            Nothing -> ChainReturn $ ResponseProtobuf MONTAGE_GET_RESPONSE $
                                            MontageGetResponse (Seq.fromList [EXISTS])
                                            obj Seq.empty
                            Just sub -> let RiakMontagePb _ pb = (ensureEval val) in
                                        let dosubs = (subrequest $ getPB buck)
                                                      pb (MSS.sub_spec sub) (MSS.params sub) in
                                        ChainGetMany dosubs obj Nothing
            Nothing            -> ChainReturn $ ResponseProtobuf MONTAGE_GET_RESPONSE $
                MontageGetResponse (Seq.fromList [MISSING]) Nothing Seq.empty

exec (ChainPut vclock buck key rec mcallback) =
    IterationRiakCommand [RiakPut vclock buck key rec] $ fromMaybe finishPut mcallback
  where
    finishPut :: (MontageRiakValue r) => [RiakResponse r] -> ChainCommand r
    finishPut (mres:[]) = ChainReturn $ ResponseProtobuf MONTAGE_PUT_RESPONSE $ makePutResponse buck key mres

exec (ChainPutMany reqs mcallback) =
    IterationRiakCommand [RiakPut vc b k r | (vc, b, k, r) <- reqs] $ fromMaybe finishPutMany mcallback
  where
    finishPutMany :: (MontageRiakValue r) => [RiakResponse r] -> ChainCommand r
    finishPutMany resps =
        ChainReturn $ ResponseProtobuf MONTAGE_PUT_MANY_RESPONSE $ MontagePutManyResponse $ Seq.fromList $ map makeObjectResp $ zip resps reqs

    makeObjectResp :: (MontageRiakValue r) => (RiakResponse r, (VectorClock, RT.Bucket, RT.Key, RiakRecord u)) -> MontagePutResponse
    makeObjectResp (mresp, (_, buck, key, _)) = makePutResponse buck key mresp

exec (ChainGetMany reqs mmaster mcallback) =
    IterationRiakCommand [RiakGet b k | (b, k) <- reqs] $ fromMaybe finishGetMany mcallback
  where
    finishGetMany :: (MontageRiakValue r) => [RiakResponse r] -> ChainCommand r
    finishGetMany rx =
        ChainReturn $ ResponseProtobuf MONTAGE_GET_RESPONSE $
            MontageGetResponse (Seq.fromList $ map doesExist rx) mmaster
                (Seq.fromList $ reverse $ foldl' makeSubObject [] $ zip reqs rx)

    doesExist (Just _) = EXISTS
    doesExist Nothing  = MISSING

    makeSubObject acc (_, Nothing) = acc
    makeSubObject acc ((b, k), Just resp) = (makeObject b k resp) : acc

exec (ChainDelete buck key mcallback) =
    IterationRiakCommand [RiakDelete buck key] $ fromMaybe finishDelete mcallback
  where
    finishDelete :: (MontageRiakValue r) => [RiakResponse r] -> ChainCommand r
    finishDelete resp@[Nothing] = ChainReturn $ ResponseProtobuf MONTAGE_DELETE_RESPONSE MontageDeleteResponse
    finishDelete _ = error "Delete should never return a result"

exec (ChainReference ref key realBucket msub) =
    let fullbuck = L.concat [refPfx, ref] in
    IterationRiakCommand [RiakGet fullbuck key] finishFollowRef
  where
    finishFollowRef :: (MontageRiakValue r) => [RiakResponse r] -> ChainCommand r
    finishFollowRef (r:[]) =
        case r of
            Just (RiakMontageReference _ realKey, vc, _) -> ChainGet realBucket realKey msub Nothing
            Nothing ->
                ChainReturn $ ResponseProtobuf MONTAGE_GET_RESPONSE $
                    MontageGetResponse Seq.empty Nothing Seq.empty

exec (ChainReferenceSet ref key realKey) =
    let fullbuck = L.concat [refPfx, ref] in
    IterationRiakCommand [RiakPut Nothing fullbuck key $ RiakMontageReference fullbuck realKey] finishSetRef
  where
    finishSetRef :: (MontageRiakValue r) => [RiakResponse r] -> ChainCommand r
    finishSetRef _ = ChainReturn $ ResponseCustom "reference-set-okay" Nothing

exec (ChainCustom cmd arg) = customCommandHandler cmd arg

exec (ChainCommandIO cmd) = ChainIterationIO cmd

-- Finally...
exec (ChainReturn resp) = IterationResponse resp


makePutResponse :: (MontageRiakValue r) => L.ByteString -> L.ByteString -> RiakResponse r -> MontagePutResponse
makePutResponse buck key mresp =
    case mresp of
        Just resp -> MontagePutResponse True $ makeObject buck key resp
        Nothing -> error "no return value on put??"
