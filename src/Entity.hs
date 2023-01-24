{-# LANGUAGE DefaultSignatures #-}
module Entity 
  (
    Entity (..),
    columnNameFor,
    maybeColumnNameFor,
    toString,
  ) where

import Data.Data ( Data )
import Database.HDBC ( SqlValue )
import RecordtypeReflection ( gFromRow, gToRow )    
import TypeInfo
import Data.Char (toLower)
import GHC.Data.Maybe (expectJust)

{--
This is the Entity class. It is a type class that is used to define the mapping between a Haskell product type in record notation and a database table.
The class has a default implementation for all methods. The default implementation uses the type information to determine a simple 1:1 mapping.

That means that the type name is used as the table name and the field names are used as the column names.
And a field named '<lowercase typeName>ID' is used as the primary key field.

The default implementation can be overridden by defining a custom instance for a type.
For example:

data Book = Book
  { book_id :: Int
  , title :: String
  , author :: String
  , year :: Int
  } deriving (Data, Show)

instance Entity Book where
  idField _         = "book_id"
  fieldsToColumns _ = [("book_id", "bookId"), ("title", "bookTitle"), ("author", "bookAuthor"), ("year", "bookYear")]
  tableName _       = "BOOK_TBL"

This will allow to persists books in a table named "BOOK_TBL" with the following columns: "bookId", "bookTitle", "bookAuthor", "bookYear".

The actual mapping to convert a database row to a Haskell value and vice versa is defined by the following methods:
class (Data a) => Entity a where
  fromRow :: [SqlValue] -> a
  toRow :: a -> [SqlValue]

The default implementation of these methods uses generics and reflection to access the field names, field types of the type and the field values of type instances.
Again, this default implementation can be overridden by defining a custom instance for a type:

instance Entity Book where
  fromRow row = Book (col 0) (col 1) (col 2) (col 3)
    where col i = fromSql (row !! i)
  toRow b = map toSql [toSql (book_id b), toSql (title b), toSql (author b), toSql (year b)]


Please note the following constraints, that are not explicietly encoded in the type class definition:

- The type must be a product type in record notation.
- The type must have exactly one constructor. 
- There must be single primary key field, compund primary keys are not supported.

--}

class (Data a) => Entity a where
  fromRow :: [SqlValue] -> a
  toRow :: a -> [SqlValue]
  idField :: a -> String
  fieldsToColumns :: a -> [(String, String)]
  tableName :: a -> String

  default fromRow :: [SqlValue] -> a
  fromRow = gFromRow

  default toRow :: a -> [SqlValue]
  toRow = gToRow

  default idField :: a -> String
  idField = idFieldName . typeInfo

  default fieldsToColumns :: a -> [(String, String)]
  fieldsToColumns x = zip (fieldNames (typeInfo x)) (fieldNames (typeInfo x))

  default tableName :: a -> String
  tableName = typeName . typeInfo

-- | A function that returns the name of the primary key column for a type 'a'.
--   By convention we are using the following name: convert the type name to lower case and append "ID".
idFieldName :: TypeInfo a -> String
idFieldName ti = map toLower (typeName ti) ++ "ID"

maybeColumnNameFor :: Entity a => a -> String -> Maybe String
maybeColumnNameFor x fieldName = lookup fieldName (fieldsToColumns x)

columnNameFor :: Entity a => a -> String -> String
columnNameFor x fieldName = expectJust 
    ("columnNameFor: " ++ toString x ++ " has no column mapping for " ++ fieldName) 
    (maybeColumnNameFor x fieldName)

toString :: (Entity a) => a -> String
toString x = typeName (typeInfo x) ++ " " ++ show (toRow x)