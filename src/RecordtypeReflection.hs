{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module RecordtypeReflection
  ( buildFromRecord,
    applyConstr,
    fieldValue,
    gFromRow,
    gToRow,
  )
where

import           Control.Monad                  (zipWithM)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State.Lazy (StateT (..))
import qualified Data.ByteString                as B
import           Data.Data                      hiding (typeRep)
import           Data.Dynamic                   (Dynamic, fromDynamic, toDyn)
import           Data.Int                       (Int32, Int64)
import           Data.List                      (elemIndex, uncons)
import           Data.Ratio                     (Ratio)
import qualified Data.Text                      as TS
import qualified Data.Text.Lazy                 as TL
import           Data.Time                      (Day, LocalTime,
                                                 NominalDiffTime, TimeOfDay,
                                                 UTCTime, ZonedTime)
import           Data.Time.Clock.POSIX          (POSIXTime)
import           Data.Word                      (Word32, Word64)
import           Database.HDBC                  (SqlValue, fromSql, toSql)
import           GHC.Data.Maybe                 (expectJust)
import           Type.Reflection                (SomeTypeRep (..), eqTypeRep,
                                                 typeRep)
import           TypeInfo

-- | A function that takes an entity and a field name as input parameters and returns the value of the field as a String.
--  Example: fieldValue (Person "John" 42) "name" = SqlString "John"
--  Example: fieldValue (Person "John" 42) "age" = SqlInt64 42
--  if the field is not present in the entity, an error is thrown.
fieldValue :: Data a => a -> String -> SqlValue
fieldValue x field =
  convertToSqlValue fieldType (valueList !! index)
  where
    ti = typeInfo x
    fieldList = fieldNames ti
    valueList = fieldValues x
    index =
      expectJust
        ("Field " ++ field ++ " is not present in type " ++ typeName ti)
        (elemIndex field fieldList)
    fieldType = fieldTypes ti !! index

fieldValues :: (Data a) => a -> [Dynamic]
fieldValues = gmapQ toDyn

gFromRow :: forall a. (Data a) => [SqlValue] -> a
gFromRow row = expectJust ("can't construct an " ++ tName ++ " instance from " ++ show row) (buildFromRecord ti row)
  where
    ti = typeInfoFromContext
    tName = typeName ti

gToRow :: (Data a) => a -> [SqlValue]
gToRow x = zipWith convertToSqlValue types values
  where
    ti = typeInfo x
    types = fieldTypes ti
    values = fieldValues x

-- | This function takes a `TypeInfo a`and a List of HDBC `SqlValue`s and returns a `Maybe a`.
--  If the conversion fails, Nothing is returned, otherwise Just a.
buildFromRecord :: (Data a) => TypeInfo a -> [SqlValue] -> Maybe a
buildFromRecord ti record = applyConstr ctor dynamicsArgs
  where
    ctor = typeConstructor ti
    types = fieldTypes ti
    dynamicsArgs =
      expectJust
        ("buildFromRecord: error in converting record " ++ show record)
        (zipWithM convertToDynamic types record)

-- | This function takes a `Constr` and a list of `Dynamic` values and returns a `Maybe a`.
--   If an `a`entity could be constructed, Just a is returned, otherwise Nothing.
--   See also https://stackoverflow.com/questions/47606189/fromconstrb-or-something-other-useful
--   for Info on how to use fromConstrM
applyConstr :: Data a => Constr -> [Dynamic] -> Maybe a
applyConstr ctor args =
  let nextField :: forall d. Data d => StateT [Dynamic] Maybe d
      nextField = StateT uncons >>= lift . fromDynamic
   in case runStateT (fromConstrM nextField ctor) args of
        Just (x, []) -> Just x
        _            -> Nothing -- runtime type error or too few / too many arguments

-- | convert a SqlValue into a Dynamic value that is backed by a value of the type represented by the SomeTypeRep parameter.
--  If conversion fails, return Nothing.
--  conversion to Dynamic is required to allow the use of fromDynamic in applyConstr
--  see also https://stackoverflow.com/questions/46992740/how-to-specify-type-of-value-via-typerep
convertToDynamic :: SomeTypeRep -> SqlValue -> Maybe Dynamic
convertToDynamic (SomeTypeRep rep) val
  | Just HRefl <- eqTypeRep rep (typeRep @Int) = Just $ toDyn (fromSql val :: Int)
  | Just HRefl <- eqTypeRep rep (typeRep @Double) = Just $ toDyn (fromSql val :: Double)
  | Just HRefl <- eqTypeRep rep (typeRep @String) = Just $ toDyn (fromSql val :: String)
  | Just HRefl <- eqTypeRep rep (typeRep @Char) = Just $ toDyn (fromSql val :: Char)
  | Just HRefl <- eqTypeRep rep (typeRep @B.ByteString) = Just $ toDyn (fromSql val :: B.ByteString)
  | Just HRefl <- eqTypeRep rep (typeRep @Word32) = Just $ toDyn (fromSql val :: Word32)
  | Just HRefl <- eqTypeRep rep (typeRep @Word64) = Just $ toDyn (fromSql val :: Word64)
  | Just HRefl <- eqTypeRep rep (typeRep @Int32) = Just $ toDyn (fromSql val :: Int32)
  | Just HRefl <- eqTypeRep rep (typeRep @Int64) = Just $ toDyn (fromSql val :: Int64)
  | Just HRefl <- eqTypeRep rep (typeRep @Integer) = Just $ toDyn (fromSql val :: Integer)
  | Just HRefl <- eqTypeRep rep (typeRep @Bool) = Just $ toDyn (fromSql val :: Bool)
  | Just HRefl <- eqTypeRep rep (typeRep @UTCTime) = Just $ toDyn (fromSql val :: UTCTime)
  | Just HRefl <- eqTypeRep rep (typeRep @POSIXTime) = Just $ toDyn (fromSql val :: POSIXTime)
  | Just HRefl <- eqTypeRep rep (typeRep @LocalTime) = Just $ toDyn (fromSql val :: LocalTime)
  | Just HRefl <- eqTypeRep rep (typeRep @ZonedTime) = Just $ toDyn (fromSql val :: ZonedTime)
  | Just HRefl <- eqTypeRep rep (typeRep @TimeOfDay) = Just $ toDyn (fromSql val :: TimeOfDay)
  | Just HRefl <- eqTypeRep rep (typeRep @Day) = Just $ toDyn (fromSql val :: Day)
  | Just HRefl <- eqTypeRep rep (typeRep @NominalDiffTime) = Just $ toDyn (fromSql val :: NominalDiffTime)
  | Just HRefl <- eqTypeRep rep (typeRep @Ratio) = Just $ toDyn (fromSql val :: Ratio Integer)
  | Just HRefl <- eqTypeRep rep (typeRep @TL.Text) = Just $ toDyn (fromSql val :: TL.Text)
  | Just HRefl <- eqTypeRep rep (typeRep @TS.Text) = Just $ toDyn (fromSql val :: TS.Text)
  | otherwise = Nothing

convertToSqlValue :: SomeTypeRep -> Dynamic -> SqlValue
convertToSqlValue (SomeTypeRep rep) dyn
  | Just HRefl <- eqTypeRep rep (typeRep @Int) = toSql (expectJust ("Not an Int: " ++ show dyn) (fromDynamic dyn) :: Int)
  | Just HRefl <- eqTypeRep rep (typeRep @Double) = toSql (expectJust ("Not a Double: " ++ show dyn) (fromDynamic dyn) :: Double)
  | Just HRefl <- eqTypeRep rep (typeRep @String) = toSql (expectJust ("Not a String: " ++ show dyn) (fromDynamic dyn) :: String)
  | Just HRefl <- eqTypeRep rep (typeRep @Char) = toSql (expectJust ("Not a Char: " ++ show dyn) (fromDynamic dyn) :: Char)
  | Just HRefl <- eqTypeRep rep (typeRep @B.ByteString) = toSql (expectJust ("Not a ByteString: " ++ show dyn) (fromDynamic dyn) :: B.ByteString)
  | Just HRefl <- eqTypeRep rep (typeRep @Word32) = toSql (expectJust ("Not a Word32: " ++ show dyn) (fromDynamic dyn) :: Word32)
  | Just HRefl <- eqTypeRep rep (typeRep @Word64) = toSql (expectJust ("Not a Word64: " ++ show dyn) (fromDynamic dyn) :: Word64)
  | Just HRefl <- eqTypeRep rep (typeRep @Int32) = toSql (expectJust ("Not an Int32: " ++ show dyn) (fromDynamic dyn) :: Int32)
  | Just HRefl <- eqTypeRep rep (typeRep @Int64) = toSql (expectJust ("Not an Int64: " ++ show dyn) (fromDynamic dyn) :: Int64)
  | Just HRefl <- eqTypeRep rep (typeRep @Integer) = toSql (expectJust ("Not an Integer: " ++ show dyn) (fromDynamic dyn) :: Integer)
  | Just HRefl <- eqTypeRep rep (typeRep @Bool) = toSql (expectJust ("Not a Bool: " ++ show dyn) (fromDynamic dyn) :: Bool)
  | Just HRefl <- eqTypeRep rep (typeRep @UTCTime) = toSql (expectJust ("Not a UTCTime: " ++ show dyn) (fromDynamic dyn) :: UTCTime)
  | Just HRefl <- eqTypeRep rep (typeRep @POSIXTime) = toSql (expectJust ("Not a PosixTime: " ++ show dyn) (fromDynamic dyn) :: POSIXTime)
  | Just HRefl <- eqTypeRep rep (typeRep @LocalTime) = toSql (expectJust ("Not a LocalTime: " ++ show dyn) (fromDynamic dyn) :: LocalTime)
  | Just HRefl <- eqTypeRep rep (typeRep @ZonedTime) = toSql (expectJust ("Not a ZonedTime: " ++ show dyn) (fromDynamic dyn) :: ZonedTime)
  | Just HRefl <- eqTypeRep rep (typeRep @TimeOfDay) = toSql (expectJust ("Not a TimeOfDay: " ++ show dyn) (fromDynamic dyn) :: TimeOfDay)
  | Just HRefl <- eqTypeRep rep (typeRep @Day) = toSql (expectJust ("Not a Day: " ++ show dyn) (fromDynamic dyn) :: Day)
  | Just HRefl <- eqTypeRep rep (typeRep @NominalDiffTime) = toSql (expectJust ("Not a NominalTimeDiff: " ++ show dyn) (fromDynamic dyn) :: NominalDiffTime)
  | Just HRefl <- eqTypeRep rep (typeRep @Ratio) = toSql (expectJust ("Not a Ratio: " ++ show dyn) (fromDynamic dyn) :: Ratio Integer)
  | Just HRefl <- eqTypeRep rep (typeRep @TL.Text) = toSql (expectJust ("Not a TL.Text: " ++ show dyn) (fromDynamic dyn) :: TL.Text)
  | Just HRefl <- eqTypeRep rep (typeRep @TS.Text) = toSql (expectJust ("Not a TS.Text: " ++ show dyn) (fromDynamic dyn) :: TS.Text)
  | otherwise = error $ "convertToSqlValue: " ++ show rep ++ " not supported"
