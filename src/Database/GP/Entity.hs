{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances, ScopedTypeVariables, DefaultSignatures, DeriveAnyClass #-}

module Database.GP.Entity
  ( Entity (..),
    columnNameFor,
    toString,
    EntityId,
    gtoRow,
    GToRow,
    GFromRow,
    maybeFieldTypeFor,
    Conn,
  )
where

import           Data.Char                        (toLower)
import           Data.Data
import           Database.GP.TypeInfo
import           Database.HDBC                    (ConnWrapper, SqlValue, fromSql)
import           GHC.Generics
import           Data.Kind
import           GHC.TypeNats
import           Data.Convertible

{--
This is the Entity class. It is a type class that is used to define the mapping
between a Haskell product type in record notation and a database table.
The class has a default implementation for all methods.
The default implementation uses the type information to determine a simple 1:1 mapping.

That means that
- the type name is used as the table name and the
- field names are used as the column names.
- A field named '<lowercase typeName>ID' is used as the primary key field.

The default implementation can be overridden by defining a custom instance for a type.

Please note the following constraints, which apply to all valid Entity type,
but that are not explicitely encoded in the type class definition:

- The type must be a product type in record notation.
- The type must have exactly one constructor.
- There must be single primary key field, compund primary keys are not supported.

--}

type Conn = ConnWrapper

class (Generic a, HasConstructor (Rep a), (HasSelectors (Rep a)) ) => Entity a where
  -- | Converts a database row to a value of type 'a'.
  fromRow :: Conn -> [SqlValue] -> IO a

  -- | Converts a value of type 'a' to a database row.
  toRow :: Conn -> a -> IO [SqlValue]

  -- | Returns the name of the primary key field for a type 'a'.
  idField :: a -> String

  -- | Returns a list of tuples that map field names to column names for a type 'a'.
  fieldsToColumns :: a -> [(String, String)]

  -- | Returns the name of the table for a type 'a'.
  tableName :: a -> String

  -- | Returns a default instance of type 'a'.
  def :: a

  -- | fromRow generic default implementation
  default fromRow :: (GFromRow (Rep a)) => Conn -> [SqlValue] -> IO a
  fromRow _conn = pure . to <$> gfromRow 

  -- | toRow generic default implementation
  default toRow :: GToRow (Rep a) => Conn -> a -> IO [SqlValue]
  toRow _ = pure . gtoRow . from

  -- | idField default implementation: the ID field is the field with the same name
  --   as the type name in lower case and appended with "ID", e.g. "bookID"
  default idField :: a -> String
  idField = idFieldName . typeInfo
    where
      idFieldName :: TypeInfo a -> String
      idFieldName ti = map toLower (constructorName ti) ++ "ID"

  -- | fieldsToColumns default implementation: the field names are used as column names
  default fieldsToColumns :: a -> [(String, String)]
  fieldsToColumns x = zip (fieldNames (typeInfo x)) (fieldNames (typeInfo x))

  -- | tableName default implementation: the type name is used as table name
  default tableName :: a -> String
  tableName = constructorName . typeInfo

  -- | def default implementation: return the default value of the generic representation
  --default def :: (GDefault (Rep a)) => a
  --def = to gdef


-- | The EntityId is a tuple of the type name and the primary key value of an Entity.
type EntityId = (String, SqlValue)

-- | A convenience function: returns the name of the column for a field of a type 'a'.
columnNameFor :: (Entity a) => a -> String -> String
columnNameFor x fieldName =
  case maybeColumnNameFor x fieldName of
    Just columnName -> columnName
    Nothing -> error ("columnNameFor: " ++ toString x ++
                      " has no column mapping for " ++ fieldName)
  where
    maybeColumnNameFor :: Entity a => a -> String -> Maybe String
    maybeColumnNameFor a field = lookup field (fieldsToColumns a)


maybeFieldTypeFor :: (Entity a) => a -> String -> Maybe TypeRep
maybeFieldTypeFor a field = lookup field (fieldsAndTypes (typeInfo a))
  where
    fieldsAndTypes :: TypeInfo a -> [(String, TypeRep)]
    fieldsAndTypes ti = zip (fieldNames ti) (fieldTypes ti)

-- | Returns a string representation of a value of type 'a'.
toString :: (Entity a) => a -> String
toString x = constructorName (typeInfo x) ++ " " ++ unwords mappedRow
  where
    mappedRow = map fromSql row
    row = [] --(gtoRow . from) x -- TODO: fix this (ie. provide a generic toString function)


-- generics based implementations for gFromRow and gToRow
-- toRow
class GToRow f where
  gtoRow :: f a -> [SqlValue]

instance GToRow U1 where
  gtoRow U1 = mempty

instance (Convertible a SqlValue) => GToRow (K1 i a) where
  gtoRow (K1 a) = pure $ convert a

instance (GToRow a, GToRow b) => GToRow (a :*: b) where
  gtoRow (a :*: b) = gtoRow a `mappend` gtoRow b

instance GToRow a => GToRow (M1 i c a) where
  gtoRow (M1 a) = gtoRow a

-- fromRow
class GFromRow f where
  gfromRow :: [SqlValue] -> f a

instance GFromRow U1 where
  --gfromRow :: forall k (a :: k). [SqlValue] -> U1 a
  gfromRow = pure U1

instance (Convertible SqlValue a) => GFromRow (K1 i a) where
  --gfromRow :: forall k (a1 :: k). [SqlValue] -> K1 i a a1
  gfromRow = K1 <$> convert . head

instance GFromRow a => GFromRow (M1 i c a) where
  --gfromRow :: forall k (a :: k -> Type) i (c :: Meta) (a1 :: k). GFromRow a => [SqlValue] -> M1 i c a a1
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


----
-- https://stackoverflow.com/questions/39619805/rep-type-in-ghc-generics
class GDefault f where
  gdef :: f a

instance GDefault U1 where
  gdef = U1

instance Entity a => GDefault (K1 i a) where
  gdef = K1 def

instance (GDefault a, GDefault b) => GDefault (a :*: b) where
  gdef = gdef :*: gdef

instance GDefault a => GDefault (M1 i c a) where
  gdef = M1 gdef
