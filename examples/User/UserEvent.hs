{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module User.UserEvent (UserEvent(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data UserEvent = UserEvent{eid :: !P'.Word32}
               deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable UserEvent where
  mergeAppend (UserEvent x'1) (UserEvent y'1) = UserEvent (P'.mergeAppend x'1 y'1)
 
instance P'.Default UserEvent where
  defaultValue = UserEvent P'.defaultValue
 
instance P'.Wire UserEvent where
  wireSize ft' self'@(UserEvent x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 13 x'1)
  wirePut ft' self'@(UserEvent x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 13 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{eid = new'Field}) (P'.wireGet 13)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> UserEvent) UserEvent where
  getVal m' f' = f' m'
 
instance P'.GPB UserEvent
 
instance P'.ReflectDescriptor UserEvent where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8]) (P'.fromDistinctAscList [8])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".User.UserEvent\", haskellPrefix = [], parentModule = [MName \"User\"], baseName = MName \"UserEvent\"}, descFilePath = [\"User\",\"UserEvent.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".User.UserEvent.eid\", haskellPrefix' = [], parentModule' = [MName \"User\",MName \"UserEvent\"], baseName' = FName \"eid\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"