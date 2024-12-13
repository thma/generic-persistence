{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.GP.Entity
  ( Entity (..),
    columnNameFor,
    gtoRow,
    GToRow,
    GFromRow,
    maybeFieldTypeFor,
    Conn (..),
    TxHandling (..),
    maybeIdFieldIndex,
    fieldIndex,
  )
where

import           Data.Char            (toLower)
import           Data.Convertible
import           Data.Kind
import           Data.List            (elemIndex)
import           Data.Typeable        (Proxy (..), TypeRep)
import           Database.GP.Conn
import           Database.GP.TypeInfo
import           Database.GP.HdbcCarveout        (SqlValue)
import           GHC.Generics
import           GHC.TypeNats

-- | This is the Entity class. It is a type class that is used to define the mapping
-- between a Haskell product type in record notation and a database table.
-- The class has a default implementation for all methods.
-- The default implementation uses the type information to determine a simple 1:1 mapping.
--
-- That means that
-- - the type name is used as the table name and the
-- - field names are used as the column names.
-- - A field named '<lowercase typeName>ID' is used as the primary key field.
--
-- The default implementation can be overridden by defining a custom instance for a type.
--
-- Please note the following constraints, which apply to all valid Entity type,
-- but that are not explicitely encoded in the type class definition:
--
-- - The type must be a product type in record notation.
-- - The type must have exactly one constructor.
-- - There must be single primary key field, compound primary keys are not supported.
class (Generic a, HasConstructor (Rep a), HasSelectors (Rep a)) => Entity a where
  -- | Converts a database row to a value of type 'a'.
  fromRow :: Conn -> [SqlValue] -> IO a

  -- | Converts a value of type 'a' to a database row.
  toRow :: Conn -> a -> IO [SqlValue]

  -- | Returns the name of the primary key field for a type 'a'.
  idField :: String

  -- | Returns a list of tuples that map field names to column names for a type 'a'.
  fieldsToColumns :: [(String, String)]

  -- | Returns the name of the table for a type 'a'.
  tableName :: String

  -- | Returns True if the primary key field for a type 'a' is autoincremented by the database.
  autoIncrement :: Bool

  -- | fromRow generic default implementation
  default fromRow :: (GFromRow (Rep a)) => Conn -> [SqlValue] -> IO a
  fromRow _conn = pure . to <$> gfromRow

  -- | toRow generic default implementation
  default toRow :: GToRow (Rep a) => Conn -> a -> IO [SqlValue]
  toRow _ = pure . gtoRow . from

  -- | idField default implementation: the ID field is the field with the same name
  --   as the type name in lower case and appended with "ID", e.g. "bookID"
  default idField :: String
  idField = map toLower (constructorName (typeInfo @a)) ++ "ID"

  -- | fieldsToColumns default implementation: the field names are used as column names
  default fieldsToColumns :: [(String, String)]
  fieldsToColumns = zip (fieldNames (typeInfo @a)) (fieldNames (typeInfo @a))

  -- | tableName default implementation: the type name is used as table name
  default tableName :: String
  tableName = constructorName (typeInfo @a)

  -- | autoIncrement default implementation: the ID field is autoincremented
  default autoIncrement :: Bool
  autoIncrement = True

-- | Returns Just index of the primary key field for a type 'a'.
--   if the type has no primary key field, Nothing is returned.
maybeIdFieldIndex :: forall a. (Entity a) => Maybe Int
maybeIdFieldIndex = elemIndex (idField @a) (fieldNames (typeInfo @a))

-- | returns the index of a field of an entity.
--   The index is the position of the field in the list of fields of the entity.
--   If no such field exists, an error is thrown.
--   The function takes an field name as parameters,
--   the type of the entity is determined by the context.
fieldIndex :: forall a. (Entity a) => String -> Int
fieldIndex fieldName =
  expectJust
    ("Field " ++ fieldName ++ " is not present in type " ++ constructorName ti)
    (elemIndex fieldName fieldList)
  where
    ti = typeInfo @a
    fieldList = fieldNames ti

expectJust :: String -> Maybe a -> a
expectJust _ (Just x)  = x
expectJust err Nothing = error ("expectJust " ++ err)

-- | A convenience function: returns the name of the column for a field of a type 'a'.
columnNameFor :: forall a. (Entity a) => String -> String
columnNameFor fieldName =
  case maybeColumnNameFor fieldName of
    Just columnName -> columnName
    Nothing ->
      error
        ( "columnNameFor: "
            ++ tableName @a
            ++ " has no column mapping for "
            ++ fieldName
        )
  where
    maybeColumnNameFor :: String -> Maybe String
    maybeColumnNameFor field = lookup field (fieldsToColumns @a)

maybeFieldTypeFor :: forall a. (Entity a) => String -> Maybe TypeRep
maybeFieldTypeFor field = lookup field (fieldsAndTypes (typeInfo @a))
  where
    fieldsAndTypes :: TypeInfo a -> [(String, TypeRep)]
    fieldsAndTypes ti = zip (fieldNames ti) (fieldTypes ti)

-- generics based implementations for gFromRow and gToRow
class GToRow f where
  gtoRow :: f a -> [SqlValue]

instance (Convertible a SqlValue) => GToRow (K1 i a) where
  gtoRow (K1 a) = pure $ convert a

instance (GToRow a, GToRow b) => GToRow (a :*: b) where
  gtoRow (a :*: b) = gtoRow a `mappend` gtoRow b

instance GToRow a => GToRow (M1 i c a) where
  gtoRow (M1 a) = gtoRow a

class GFromRow f where
  gfromRow :: [SqlValue] -> f a

instance (Convertible SqlValue a) => GFromRow (K1 i a) where
  gfromRow = K1 <$> convert . head

instance GFromRow a => GFromRow (M1 i c a) where
  gfromRow = M1 <$> gfromRow

-- | This instance is the most interesting one. It splits the list of
-- 'SqlValue's into two parts, one for the first field and one for the
-- rest. Then it uses the 'GFromRow' instance for the first field to
-- convert the first part of the list and the 'GFromRow' instance for
-- the rest of the fields to convert the second part of the list.
-- Finally, it combines the two results using the ':*:' constructor.
-- https://stackoverflow.com/questions/75485429/how-to-use-ghc-generics-to-convert-from-product-data-types-to-a-list-of-sqlvalue/75485650#75485650
instance (KnownNat (NumFields f), GFromRow f, GFromRow g) => GFromRow (f :*: g) where
  gfromRow row = gfromRow rowf :*: gfromRow rowg
    where
      (rowf, rowg) = splitAt fNumFields row
      fNumFields = fromIntegral (natVal (Proxy :: Proxy (NumFields f)))

type family NumFields (f :: Type -> Type) :: Nat where
  NumFields (M1 i c f) = 1
  NumFields (f :*: g) = NumFields f + NumFields g
