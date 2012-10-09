{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Network.Riak.Montage.Proto.Montage.MontageGetStatus (MontageGetStatus(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data MontageGetStatus = EXISTS
                      | MISSING
                      | ERROR
                      deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable MontageGetStatus
 
instance Prelude'.Bounded MontageGetStatus where
  minBound = EXISTS
  maxBound = ERROR
 
instance P'.Default MontageGetStatus where
  defaultValue = EXISTS
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe MontageGetStatus
toMaybe'Enum 1 = Prelude'.Just EXISTS
toMaybe'Enum 2 = Prelude'.Just MISSING
toMaybe'Enum 3 = Prelude'.Just ERROR
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum MontageGetStatus where
  fromEnum EXISTS = 1
  fromEnum MISSING = 2
  fromEnum ERROR = 3
  toEnum
   = P'.fromMaybe
      (Prelude'.error "hprotoc generated code: toEnum failure for type Network.Riak.Montage.Proto.Montage.MontageGetStatus")
      . toMaybe'Enum
  succ EXISTS = MISSING
  succ MISSING = ERROR
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Network.Riak.Montage.Proto.Montage.MontageGetStatus"
  pred MISSING = EXISTS
  pred ERROR = MISSING
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Network.Riak.Montage.Proto.Montage.MontageGetStatus"
 
instance P'.Wire MontageGetStatus where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB MontageGetStatus
 
instance P'.MessageAPI msg' (msg' -> MontageGetStatus) MontageGetStatus where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum MontageGetStatus where
  reflectEnum = [(1, "EXISTS", EXISTS), (2, "MISSING", MISSING), (3, "ERROR", ERROR)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".Montage.MontageGetStatus") ["Network", "Riak", "Montage", "Proto"] ["Montage"] "MontageGetStatus")
      ["Network", "Riak", "Montage", "Proto", "Montage", "MontageGetStatus.hs"]
      [(1, "EXISTS"), (2, "MISSING"), (3, "ERROR")]