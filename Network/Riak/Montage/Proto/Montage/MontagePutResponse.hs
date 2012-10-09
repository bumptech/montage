{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Network.Riak.Montage.Proto.Montage.MontagePutResponse (MontagePutResponse(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Network.Riak.Montage.Proto.Montage.MontageObject as Montage (MontageObject)
 
data MontagePutResponse = MontagePutResponse{modified :: !P'.Bool, object :: !Montage.MontageObject}
                        deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable MontagePutResponse where
  mergeAppend (MontagePutResponse x'1 x'2) (MontagePutResponse y'1 y'2)
   = MontagePutResponse (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default MontagePutResponse where
  defaultValue = MontagePutResponse P'.defaultValue P'.defaultValue
 
instance P'.Wire MontagePutResponse where
  wireSize ft' self'@(MontagePutResponse x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 8 x'1 + P'.wireSizeReq 1 11 x'2)
  wirePut ft' self'@(MontagePutResponse x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 8 x'1
             P'.wirePutReq 18 11 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{modified = new'Field}) (P'.wireGet 8)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{object = P'.mergeAppend (object old'Self) (new'Field)}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> MontagePutResponse) MontagePutResponse where
  getVal m' f' = f' m'
 
instance P'.GPB MontagePutResponse
 
instance P'.ReflectDescriptor MontagePutResponse where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 18]) (P'.fromDistinctAscList [8, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Montage.MontagePutResponse\", haskellPrefix = [MName \"Network\",MName \"Riak\",MName \"Montage\",MName \"Proto\"], parentModule = [MName \"Montage\"], baseName = MName \"MontagePutResponse\"}, descFilePath = [\"Network\",\"Riak\",\"Montage\",\"Proto\",\"Montage\",\"MontagePutResponse.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Montage.MontagePutResponse.modified\", haskellPrefix' = [MName \"Network\",MName \"Riak\",MName \"Montage\",MName \"Proto\"], parentModule' = [MName \"Montage\",MName \"MontagePutResponse\"], baseName' = FName \"modified\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Montage.MontagePutResponse.object\", haskellPrefix' = [MName \"Network\",MName \"Riak\",MName \"Montage\",MName \"Proto\"], parentModule' = [MName \"Montage\",MName \"MontagePutResponse\"], baseName' = FName \"object\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Montage.MontageObject\", haskellPrefix = [MName \"Network\",MName \"Riak\",MName \"Montage\",MName \"Proto\"], parentModule = [MName \"Montage\"], baseName = MName \"MontageObject\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"