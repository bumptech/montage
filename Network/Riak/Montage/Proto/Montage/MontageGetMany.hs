{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Network.Riak.Montage.Proto.Montage.MontageGetMany (MontageGetMany(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Network.Riak.Montage.Proto.Montage.MontageGet as Montage (MontageGet)
 
data MontageGetMany = MontageGetMany{gets :: !(P'.Seq Montage.MontageGet)}
                    deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable MontageGetMany where
  mergeAppend (MontageGetMany x'1) (MontageGetMany y'1) = MontageGetMany (P'.mergeAppend x'1 y'1)
 
instance P'.Default MontageGetMany where
  defaultValue = MontageGetMany P'.defaultValue
 
instance P'.Wire MontageGetMany where
  wireSize ft' self'@(MontageGetMany x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1)
  wirePut ft' self'@(MontageGetMany x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 10 11 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{gets = P'.append (gets old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> MontageGetMany) MontageGetMany where
  getVal m' f' = f' m'
 
instance P'.GPB MontageGetMany
 
instance P'.ReflectDescriptor MontageGetMany where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Montage.MontageGetMany\", haskellPrefix = [MName \"Network\",MName \"Riak\",MName \"Montage\",MName \"Proto\"], parentModule = [MName \"Montage\"], baseName = MName \"MontageGetMany\"}, descFilePath = [\"Network\",\"Riak\",\"Montage\",\"Proto\",\"Montage\",\"MontageGetMany.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Montage.MontageGetMany.gets\", haskellPrefix' = [MName \"Network\",MName \"Riak\",MName \"Montage\",MName \"Proto\"], parentModule' = [MName \"Montage\",MName \"MontageGetMany\"], baseName' = FName \"gets\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Montage.MontageGet\", haskellPrefix = [MName \"Network\",MName \"Riak\",MName \"Montage\",MName \"Proto\"], parentModule = [MName \"Montage\"], baseName = MName \"MontageGet\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"