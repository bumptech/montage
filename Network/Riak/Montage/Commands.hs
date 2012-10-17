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
    finishGet _ = error "Got unexpected value back from Riak"

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

exec (ChainReference ref key realBucket msub) =
    IterationRiakCommand [RiakGet ref key] finishFollowRef
  where
    finishFollowRef :: (MontageRiakValue r) => [RiakResponse r] -> ChainCommand r
    finishFollowRef (r:[]) =
        case r of
            Just (val, _, _) -> case getReferenceKey val of
                Just realKey -> ChainGet realBucket realKey msub Nothing
                Nothing -> nullResp
            Just _ -> error "Error in reference fetch"
            Nothing -> nullResp
    finishFollowRef _ = error "Unexpected result back from RiakGet"

    nullResp = ChainReturn $ ResponseProtobuf MONTAGE_GET_RESPONSE $
                    MontageGetResponse Seq.empty Nothing Seq.empty


exec (ChainCustom cmd arg) = customCommandHandler cmd arg

exec (ChainCommandIO cmd) = ChainIterationIO cmd

exec (ChainSub _ _ _) = error "Cannot handle ChainSub commands"
-- Finally...
exec (ChainReturn resp) = IterationResponse resp


makePutResponse :: (MontageRiakValue r) => L.ByteString -> L.ByteString -> RiakResponse r -> MontagePutResponse
makePutResponse buck key mresp =
    case mresp of
        Just resp -> MontagePutResponse True $ makeObject buck key resp
        Nothing -> error "no return value on put??"
