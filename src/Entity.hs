{-# LANGUAGE DefaultSignatures #-}

module Entity
  ( Entity (..),
    columnNameFor,
    fieldTypeFor,
    maybeFieldTypeFor,
    toString,
    evidence,
    evidenceFrom,
  )
where

import           Data.Char            (toLower)
import           Data.Data            (Data, TypeRep, fromConstr)
import           Database.HDBC        (SqlValue, fromSql)
import           RecordtypeReflection (gFromRow, gToRow)
import           TypeInfo             (TypeInfo (fieldNames, fieldTypes, typeConstructor), typeInfo, typeName, typeInfoFromContext)

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

class (Data a) => Entity a where
  -- | Converts a database row to a value of type 'a'.
  fromRow :: [SqlValue] -> a

  -- | Converts a value of type 'a' to a database row.
  toRow :: a -> [SqlValue]

  -- | Returns the name of the primary key field for a type 'a'.
  idField :: a -> String

  -- | Returns a list of tuples that map field names to column names for a type 'a'.
  fieldsToColumns :: a -> [(String, String)]

  -- | Returns the name of the table for a type 'a'.
  tableName :: a -> String

  -- | generic default implementation
  default fromRow :: [SqlValue] -> a
  fromRow = gFromRow

  -- | generic default implementation
  default toRow :: a -> [SqlValue]
  toRow = gToRow

  -- | default implementation: the ID field is the field with the same name
  --   as the type name in lower case and appended with "ID", e.g. "bookID"
  default idField :: a -> String
  idField = idFieldName . typeInfo
    where
      idFieldName :: TypeInfo a -> String
      idFieldName ti = map toLower (typeName ti) ++ "ID"

  -- | default implementation: the field names are used as column names
  default fieldsToColumns :: a -> [(String, String)]
  fieldsToColumns x = zip (fieldNames (typeInfo x)) (fieldNames (typeInfo x))

  -- | default implementation: the type name is used as table name
  default tableName :: a -> String
  tableName = typeName . typeInfo

-- | A convenience function: returns the name of the column for a field of a type 'a'.
columnNameFor :: Entity a => a -> String -> String
columnNameFor x fieldName =
  case maybeColumnNameFor x fieldName of
    Just columnName -> columnName
    Nothing -> error ("columnNameFor: " ++ toString x ++ 
                      " has no column mapping for " ++ fieldName)
  where
    maybeColumnNameFor :: Entity a => a -> String -> Maybe String
    maybeColumnNameFor a field = lookup field (fieldsToColumns a)

-- | A convenience function: returns the TypeRep of a field of a type 'a'.  
fieldTypeFor :: Entity a => a -> String -> TypeRep
fieldTypeFor x fieldName =
  case maybeFieldTypeFor x fieldName of
    Just typeRep -> typeRep
    Nothing -> error ("fieldTypeFor: " ++ toString x ++ 
                      " has no field " ++ fieldName)

maybeFieldTypeFor :: Entity a => a -> String -> Maybe TypeRep
maybeFieldTypeFor a field = lookup field (fieldsAndTypes (typeInfo a))
  where
    fieldsAndTypes :: TypeInfo a -> [(String, TypeRep)]
    fieldsAndTypes ti = zip (fieldNames ti) (fieldTypes ti)

-- | Returns a string representation of a value of type 'a'.
toString :: (Entity a) => a -> String
toString x = typeName (typeInfo x) ++ " " ++ unwords mappedRow
  where
    mappedRow = map fromSql (toRow x)

-- | A convenience function: returns an evidence instance of type 'a'.
--   This is useful for type inference where no instance is available.
evidence :: forall a. (Entity a) => a 
evidence = evidenceFrom ti
  where 
    ti = typeInfoFromContext :: TypeInfo a


evidenceFrom :: forall a. (Entity a) => TypeInfo a -> a
evidenceFrom = fromConstr . typeConstructor