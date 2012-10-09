{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Network.Riak.Montage.Proto.Montage.MontageWireMessages (MontageWireMessages(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data MontageWireMessages = MONTAGE_GET
                         | MONTAGE_GET_REFERENCE
                         | MONTAGE_GET_RESPONSE
                         | MONTAGE_COMMAND
                         | MONTAGE_COMMAND_RESPONSE
                         | MONTAGE_PUT
                         | MONTAGE_PUT_RESPONSE
                         | MONTAGE_GET_MANY
                         | MONTAGE_SET_REFERENCE
                         | MONTAGE_PUT_MANY
                         | MONTAGE_PUT_MANY_RESPONSE
                         | MONTAGE_ERROR
                         | MONTAGE_DELETE
                         | MONTAGE_DELETE_RESPONSE
                         deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable MontageWireMessages
 
instance Prelude'.Bounded MontageWireMessages where
  minBound = MONTAGE_GET
  maxBound = MONTAGE_DELETE_RESPONSE
 
instance P'.Default MontageWireMessages where
  defaultValue = MONTAGE_GET
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe MontageWireMessages
toMaybe'Enum 1 = Prelude'.Just MONTAGE_GET
toMaybe'Enum 2 = Prelude'.Just MONTAGE_GET_REFERENCE
toMaybe'Enum 3 = Prelude'.Just MONTAGE_GET_RESPONSE
toMaybe'Enum 4 = Prelude'.Just MONTAGE_COMMAND
toMaybe'Enum 5 = Prelude'.Just MONTAGE_COMMAND_RESPONSE
toMaybe'Enum 6 = Prelude'.Just MONTAGE_PUT
toMaybe'Enum 7 = Prelude'.Just MONTAGE_PUT_RESPONSE
toMaybe'Enum 8 = Prelude'.Just MONTAGE_GET_MANY
toMaybe'Enum 9 = Prelude'.Just MONTAGE_SET_REFERENCE
toMaybe'Enum 10 = Prelude'.Just MONTAGE_PUT_MANY
toMaybe'Enum 11 = Prelude'.Just MONTAGE_PUT_MANY_RESPONSE
toMaybe'Enum 12 = Prelude'.Just MONTAGE_ERROR
toMaybe'Enum 13 = Prelude'.Just MONTAGE_DELETE
toMaybe'Enum 14 = Prelude'.Just MONTAGE_DELETE_RESPONSE
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum MontageWireMessages where
  fromEnum MONTAGE_GET = 1
  fromEnum MONTAGE_GET_REFERENCE = 2
  fromEnum MONTAGE_GET_RESPONSE = 3
  fromEnum MONTAGE_COMMAND = 4
  fromEnum MONTAGE_COMMAND_RESPONSE = 5
  fromEnum MONTAGE_PUT = 6
  fromEnum MONTAGE_PUT_RESPONSE = 7
  fromEnum MONTAGE_GET_MANY = 8
  fromEnum MONTAGE_SET_REFERENCE = 9
  fromEnum MONTAGE_PUT_MANY = 10
  fromEnum MONTAGE_PUT_MANY_RESPONSE = 11
  fromEnum MONTAGE_ERROR = 12
  fromEnum MONTAGE_DELETE = 13
  fromEnum MONTAGE_DELETE_RESPONSE = 14
  toEnum
   = P'.fromMaybe
      (Prelude'.error "hprotoc generated code: toEnum failure for type Network.Riak.Montage.Proto.Montage.MontageWireMessages")
      . toMaybe'Enum
  succ MONTAGE_GET = MONTAGE_GET_REFERENCE
  succ MONTAGE_GET_REFERENCE = MONTAGE_GET_RESPONSE
  succ MONTAGE_GET_RESPONSE = MONTAGE_COMMAND
  succ MONTAGE_COMMAND = MONTAGE_COMMAND_RESPONSE
  succ MONTAGE_COMMAND_RESPONSE = MONTAGE_PUT
  succ MONTAGE_PUT = MONTAGE_PUT_RESPONSE
  succ MONTAGE_PUT_RESPONSE = MONTAGE_GET_MANY
  succ MONTAGE_GET_MANY = MONTAGE_SET_REFERENCE
  succ MONTAGE_SET_REFERENCE = MONTAGE_PUT_MANY
  succ MONTAGE_PUT_MANY = MONTAGE_PUT_MANY_RESPONSE
  succ MONTAGE_PUT_MANY_RESPONSE = MONTAGE_ERROR
  succ MONTAGE_ERROR = MONTAGE_DELETE
  succ MONTAGE_DELETE = MONTAGE_DELETE_RESPONSE
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Network.Riak.Montage.Proto.Montage.MontageWireMessages"
  pred MONTAGE_GET_REFERENCE = MONTAGE_GET
  pred MONTAGE_GET_RESPONSE = MONTAGE_GET_REFERENCE
  pred MONTAGE_COMMAND = MONTAGE_GET_RESPONSE
  pred MONTAGE_COMMAND_RESPONSE = MONTAGE_COMMAND
  pred MONTAGE_PUT = MONTAGE_COMMAND_RESPONSE
  pred MONTAGE_PUT_RESPONSE = MONTAGE_PUT
  pred MONTAGE_GET_MANY = MONTAGE_PUT_RESPONSE
  pred MONTAGE_SET_REFERENCE = MONTAGE_GET_MANY
  pred MONTAGE_PUT_MANY = MONTAGE_SET_REFERENCE
  pred MONTAGE_PUT_MANY_RESPONSE = MONTAGE_PUT_MANY
  pred MONTAGE_ERROR = MONTAGE_PUT_MANY_RESPONSE
  pred MONTAGE_DELETE = MONTAGE_ERROR
  pred MONTAGE_DELETE_RESPONSE = MONTAGE_DELETE
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Network.Riak.Montage.Proto.Montage.MontageWireMessages"
 
instance P'.Wire MontageWireMessages where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB MontageWireMessages
 
instance P'.MessageAPI msg' (msg' -> MontageWireMessages) MontageWireMessages where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum MontageWireMessages where
  reflectEnum
   = [(1, "MONTAGE_GET", MONTAGE_GET), (2, "MONTAGE_GET_REFERENCE", MONTAGE_GET_REFERENCE),
      (3, "MONTAGE_GET_RESPONSE", MONTAGE_GET_RESPONSE), (4, "MONTAGE_COMMAND", MONTAGE_COMMAND),
      (5, "MONTAGE_COMMAND_RESPONSE", MONTAGE_COMMAND_RESPONSE), (6, "MONTAGE_PUT", MONTAGE_PUT),
      (7, "MONTAGE_PUT_RESPONSE", MONTAGE_PUT_RESPONSE), (8, "MONTAGE_GET_MANY", MONTAGE_GET_MANY),
      (9, "MONTAGE_SET_REFERENCE", MONTAGE_SET_REFERENCE), (10, "MONTAGE_PUT_MANY", MONTAGE_PUT_MANY),
      (11, "MONTAGE_PUT_MANY_RESPONSE", MONTAGE_PUT_MANY_RESPONSE), (12, "MONTAGE_ERROR", MONTAGE_ERROR),
      (13, "MONTAGE_DELETE", MONTAGE_DELETE), (14, "MONTAGE_DELETE_RESPONSE", MONTAGE_DELETE_RESPONSE)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Montage.MontageWireMessages") ["Network", "Riak", "Montage", "Proto"] ["Montage"]
        "MontageWireMessages")
      ["Network", "Riak", "Montage", "Proto", "Montage", "MontageWireMessages.hs"]
      [(1, "MONTAGE_GET"), (2, "MONTAGE_GET_REFERENCE"), (3, "MONTAGE_GET_RESPONSE"), (4, "MONTAGE_COMMAND"),
       (5, "MONTAGE_COMMAND_RESPONSE"), (6, "MONTAGE_PUT"), (7, "MONTAGE_PUT_RESPONSE"), (8, "MONTAGE_GET_MANY"),
       (9, "MONTAGE_SET_REFERENCE"), (10, "MONTAGE_PUT_MANY"), (11, "MONTAGE_PUT_MANY_RESPONSE"), (12, "MONTAGE_ERROR"),
       (13, "MONTAGE_DELETE"), (14, "MONTAGE_DELETE_RESPONSE")]