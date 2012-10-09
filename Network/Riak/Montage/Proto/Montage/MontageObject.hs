{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Network.Riak.Montage.Proto.Montage.MontageObject (MontageObject(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data MontageObject = MontageObject{vclock :: !(P'.Maybe P'.ByteString), bucket :: !P'.ByteString, key :: !P'.ByteString,
                                   data' :: !P'.ByteString, fetch_resolutions :: !(P'.Maybe P'.Word32)}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable MontageObject where
  mergeAppend (MontageObject x'1 x'2 x'3 x'4 x'5) (MontageObject y'1 y'2 y'3 y'4 y'5)
   = MontageObject (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
 
instance P'.Default MontageObject where
  defaultValue = MontageObject P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire MontageObject where
  wireSize ft' self'@(MontageObject x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 12 x'1 + P'.wireSizeReq 1 12 x'2 + P'.wireSizeReq 1 12 x'3 + P'.wireSizeReq 1 12 x'4 +
             P'.wireSizeOpt 1 13 x'5)
  wirePut ft' self'@(MontageObject x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 12 x'1
             P'.wirePutReq 18 12 x'2
             P'.wirePutReq 26 12 x'3
             P'.wirePutReq 34 12 x'4
             P'.wirePutOpt 40 13 x'5
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{vclock = Prelude'.Just new'Field}) (P'.wireGet 12)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{bucket = new'Field}) (P'.wireGet 12)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{key = new'Field}) (P'.wireGet 12)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{data' = new'Field}) (P'.wireGet 12)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{fetch_resolutions = Prelude'.Just new'Field}) (P'.wireGet 13)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> MontageObject) MontageObject where
  getVal m' f' = f' m'
 
instance P'.GPB MontageObject
 
instance P'.ReflectDescriptor MontageObject where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [18, 26, 34]) (P'.fromDistinctAscList [10, 18, 26, 34, 40])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Montage.MontageObject\", haskellPrefix = [MName \"Network\",MName \"Riak\",MName \"Montage\",MName \"Proto\"], parentModule = [MName \"Montage\"], baseName = MName \"MontageObject\"}, descFilePath = [\"Network\",\"Riak\",\"Montage\",\"Proto\",\"Montage\",\"MontageObject.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Montage.MontageObject.vclock\", haskellPrefix' = [MName \"Network\",MName \"Riak\",MName \"Montage\",MName \"Proto\"], parentModule' = [MName \"Montage\",MName \"MontageObject\"], baseName' = FName \"vclock\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Montage.MontageObject.bucket\", haskellPrefix' = [MName \"Network\",MName \"Riak\",MName \"Montage\",MName \"Proto\"], parentModule' = [MName \"Montage\",MName \"MontageObject\"], baseName' = FName \"bucket\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Montage.MontageObject.key\", haskellPrefix' = [MName \"Network\",MName \"Riak\",MName \"Montage\",MName \"Proto\"], parentModule' = [MName \"Montage\",MName \"MontageObject\"], baseName' = FName \"key\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Montage.MontageObject.data\", haskellPrefix' = [MName \"Network\",MName \"Riak\",MName \"Montage\",MName \"Proto\"], parentModule' = [MName \"Montage\",MName \"MontageObject\"], baseName' = FName \"data'\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Montage.MontageObject.fetch_resolutions\", haskellPrefix' = [MName \"Network\",MName \"Riak\",MName \"Montage\",MName \"Proto\"], parentModule' = [MName \"Montage\",MName \"MontageObject\"], baseName' = FName \"fetch_resolutions\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"