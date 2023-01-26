{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module RecordtypeReflection
  ( buildFromRecord,
    applyConstr,
    fieldValueAsString,
    fieldValues,
    gshow
  )
where

import           Control.Monad                  (zipWithM)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State.Lazy ( StateT(..) ) 
import           Data.Data                      hiding (typeRep)
import           Data.Dynamic                   (Dynamic, fromDynamic, toDyn)
import           Data.List                      (elemIndex, uncons)
import           Database.HDBC                  (SqlValue, fromSql)
import           GHC.Data.Maybe                 (expectJust)
import           Type.Reflection                (SomeTypeRep (..), eqTypeRep,
                                                 typeRep)
import           TypeInfo                       
import           Data.Generics.Aliases          (extQ)

import qualified Data.ByteString as B
import Data.Word ( Word32, Word64 )
import Data.Int ( Int32, Int64 )
import Data.Time
    ( NominalDiffTime, Day, LocalTime, TimeOfDay, UTCTime, ZonedTime ) 
import Data.Time.Clock.POSIX ( POSIXTime )
import Data.Ratio ( Ratio )
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL


-- | A function that takes an entity and a field name as input parameters and returns the value of the field as a String.
--  Example: fieldValueAsString (Person "John" 42) "name" = "John"
--  Example: fieldValueAsString (Person "John" 42) "age" = "42"
--  if the field is not present in the entity, an error is thrown.
fieldValueAsString :: Data a => a -> String -> String
fieldValueAsString x field =
  valueList !! index
  where
    fieldList = fieldNames x
    valueList = fieldValues x
    index =
      expectJust
        ("Field " ++ field ++ " is not present in type " ++ typeName x)
        (elemIndex field fieldList)

-- | A function that take an entity as input paraemeter and returns a list of 
--   Strings representing the values of all fields of the entity.
--   Example: fieldValues (Person "John" 42) = ["John", "42"]
fieldValues :: (Data a) => a -> [String]
fieldValues = gmapQ gshow

-- | This function takes a `TypeInfo a`and a List of HDBC `SqlValue`s and returns a `Maybe a`.
--  If the conversion fails, Nothing is returned, otherwise Just a.
buildFromRecord :: (Data a) => TypeInfo a -> [SqlValue] -> Maybe a
buildFromRecord ti record = applyConstr ctor dynamicsArgs
  where
    ctor = typeConstructor ti
    types = map fieldType (typeFields ti)
    dynamicsArgs =
      expectJust
        ("buildFromRecord: error in converting record " ++ show record)
        (zipWithM convert types record)

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
convert :: SomeTypeRep -> SqlValue -> Maybe Dynamic
convert (SomeTypeRep rep) val
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

-- | Generic show: taken from syb package and https://chrisdone.com/posts/data-typeable/
gshow :: Data a => a -> String
gshow x = gshows x ""

gshows :: Data a => a -> ShowS
gshows = render `extQ` (shows :: String -> ShowS)
  where
    render t
      | isTuple =
          showChar '('
            . drop 1
            . commaSlots
            . showChar ')'
      | isNull = showString "[]"
      | isList =
          showChar '['
            . drop 1
            . listSlots
            . showChar ']'
      | otherwise =
          constructor
            . slots
      where
        constructor = showString . showConstr . toConstr $ t
        slots = foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t
        commaSlots = foldr (.) id . gmapQ ((showChar ',' .) . gshows) $ t
        listSlots = foldr (.) id . init . gmapQ ((showChar ',' .) . gshows) $ t
        isTuple = all (== ',') (filter (not . flip elem "()") (constructor ""))
        isNull = all (`elem` "[]") (constructor "")
        isList = constructor "" == "(:)"