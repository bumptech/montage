{-# LANGUAGE BangPatterns #-}
module Network.Riak.Montage.Commands where

import Network.Riak.Montage.Proto.Montage.MontageWireMessages
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Network.Riak.Types as RT
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy.Char8 as L

import Network.Riak.Types

import Network.Riak.Montage.Proto.Montage.MontageObject (MontageObject(MontageObject))
import Network.Riak.Montage.Proto.Montage.MontageGetResponse
import Network.Riak.Montage.Proto.Montage.MontagePutResponse
import Network.Riak.Montage.Proto.Montage.MontagePutManyResponse
import Network.Riak.Montage.Proto.Montage.MontageGetStatus
import Network.Riak.Montage.Proto.Montage.MontageSubrequestSpec as MSS
import Network.Riak.Montage.Proto.Montage.MontageDeleteResponse

import Network.Riak.Montage.Types


makeObject :: (MontageRiakValue r) => L.ByteString -> L.ByteString -> (RiakRecord r, VClock, Maybe Int) -> MontageObject
makeObject buck key (rec, vclock, resolutions) =
    MontageObject
        (Just $ RT.fromVClock $ vclock)
        buck
        key
        (riakSerialize rec)
        (fmap fromIntegral resolutions)



exec :: ( MontageRiakValue r) => ChainCommand r -> ChainIteration r
exec (ChainGet buck key mfollow) =
    IterationRiakCommand [RiakGet buck key] $ fromMaybe finishGet mfollow
  where
    finishGet :: (MontageRiakValue r) => [RiakResponse r] -> ChainCommand r
    finishGet (mres:[]) =
        case mres of
            Just resp@(val, _, _) -> getResp (Seq.fromList [EXISTS]) obj Seq.empty
              where obj = (Just $ makeObject buck key resp)
            Nothing -> getResp (Seq.fromList [MISSING]) Nothing Seq.empty
    finishGet _ = error "Got unexpected value back from Riak"

    getResp status master subs = ChainReturn $ ResponseProtobuf MONTAGE_GET_RESPONSE $
        MontageGetResponse status master subs

exec (ChainPut vclock buck key rec mcallback) =
    IterationRiakCommand [RiakPut vclock buck key rec] $ fromMaybe finishPut mcallback
  where
    finishPut :: (MontageRiakValue r) => [RiakResponse r] -> ChainCommand r
    finishPut (mres:[]) = ChainReturn $ ResponseProtobuf MONTAGE_PUT_RESPONSE $ makePutResponse buck key mres
    finishPut _  = error "Got unexpected value back from Riak"

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
    finishDelete [Nothing] = ChainReturn $ ResponseProtobuf MONTAGE_DELETE_RESPONSE MontageDeleteResponse
    finishDelete _ = error "Delete should never return a result"

exec (ChainReference ref key targets) =
    IterationRiakCommand [RiakGet ref key] finishFollowRef
  where
    finishFollowRef :: (MontageRiakValue r) => [RiakResponse r] -> ChainCommand r
    finishFollowRef (r:[]) =
      case r of
            Just resp@(val, _, _) -> case getReferenceKey (ensureEval val) of
                Just realKey -> let firstLookup = (Just $ makeObject ref key resp) in
                    ChainGetMany [ (t, realKey) | t <- targets ] firstLookup Nothing
                Nothing -> nullResp
            Nothing -> nullResp
    finishFollowRef _ = error "Unexpected result back from RiakGet"

    nullResp = ChainReturn $ ResponseProtobuf MONTAGE_GET_RESPONSE $
                    MontageGetResponse Seq.empty Nothing Seq.empty


exec (ChainCustom cmd arg) = customCommandHandler cmd arg

exec (ChainCommandIO cmd) = ChainIterationIO cmd

-- Finally...
exec (ChainReturn resp) = IterationResponse resp


makePutResponse :: (MontageRiakValue r) => L.ByteString -> L.ByteString -> RiakResponse r -> MontagePutResponse
makePutResponse buck key mresp =
    case mresp of
        Just resp -> MontagePutResponse True $ makeObject buck key resp
        Nothing -> error "no return value on put??"
