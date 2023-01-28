# GenericPersistence - A Haskell persistence layer using Generics and Reflection

![GP Logo](gp-logo-300.png)

## Introduction

GenericPersistence is a minimalistic Haskell persistence layer (on top of HDBC). 
The approach relies on Generics (`Data.Data`, `Data.Typeable`) and Reflection (`Type.Reflection`).

The *functional goal* of the persistence layer is to provide hassle-free RDBMS persistence for Haskell data types in 
Record notation (for brevity I call them *Entities*).

That is, it provides means for inserting, updating, deleting and quering such enties to/from relational databases.

The main *design goal* is to minimize the *boilerplate* code required:

- no manual instantiation of type classes
- no implementation of encoders/decoders
- no special naming convention for types and their attributes 
- no special types to define entities and attributes
- no Template Haskell scaffolding of glue code

In an ideal world we would be able to take any POHO (Plain old Haskell Object) 
and persist it to any RDBMS without any additional effort.

A lot of things are still missing:

- A query language
- Handling of relationships between entities (1:1, 1:n, n:m)
- Handling of nested transactions
- Handling auto-incrementing primary keys
- Caching
- ...


## Short demo

Here now follows a short demo that shows how the library looks and feels from the user's point of view.

```haskell
{-# LANGUAGE DeriveAnyClass     #-}  -- allows automatic derivation from Entity type class
module Main (main) where

import           Data.Data             (Data)
import           Database.HDBC         (disconnect, fromSql, toSql)
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           GenericPersistence    (delete, persist, retrieveAll, retrieveById, Entity(..), setupTableFor) 

-- | A data type with several fields, using record syntax.
data Person = Person
  { personID :: Int,
    name     :: String,
    age      :: Int,
    address  :: String
  }
  deriving (Data, Entity, Show)

data Book = Book
  { book_id :: Int,
    title   :: String,
    author  :: String,
    year    :: Int
  }
  deriving (Data, Show)

instance Entity Book where
  idField _ = "book_id"
  fieldsToColumns _ = [("title", "bookTitle"), ("author", "bookAuthor"), ("year", "bookYear"), ("book_id", "bookId")]
  tableName _ = "BOOK_TBL"
  fromRow row = Book (col 0) (col 1) (col 2) (col 3)
    where
      col i = fromSql (row !! i)
  toRow b = [toSql (book_id b), toSql (title b), toSql (author b), toSql (year b)]


main :: IO ()
main = do
  -- connect to a database
  conn <- connectSqlite3 "sqlite.db"

  -- set up tables for Person and Book
  _ <- setupTableFor conn :: IO Person
  _ <- setupTableFor conn :: IO Book

  -- create some demo data
  let alice = Person 123456 "Alice" 25 "123 Main St"
      book  = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937

  -- insert a Person into a database
  persist conn alice

  -- insert a second Person in a database
  persist conn alice {personID = 123457, name = "Bob"}

  -- update a Person
  persist conn alice {address = "Elmstreet 1"}

  -- select a Person from a database
  alice' <- retrieveById conn (123456 :: Int) :: IO Person
  print alice'

  -- select all Persons from a database
  allPersons <- retrieveAll conn :: IO [Person]
  print allPersons

  -- delete a Person from a database
  delete conn alice

  -- select all Persons from a database
  allPersons' <- retrieveAll conn :: IO [Person]
  print allPersons'

  let book2 = Book {book_id = 2, title = "The Lord of the Ring", author = "J.R.R. Tolkien", year = 1954}

  persist conn book
  persist conn book2
  allBooks <- retrieveAll conn :: IO [Book]
  print allBooks

  persist conn book2 {title = "The Lord of the Rings"}
  delete conn book

  allBooks' <- retrieveAll conn :: IO [Book]
  print allBooks'

  -- close connection
  disconnect conn

```

## The Entity type class

```haskell
{-# LANGUAGE DefaultSignatures #-}

module Entity
  ( Entity (..),
    columnNameFor,
    toString,
  )
where

import           Data.Char            (toLower)
import           Data.Data            (Data)
import           Database.HDBC        (SqlValue, fromSql)
import           RecordtypeReflection (gFromRow, gToRow)
import           TypeInfo             (TypeInfo (fieldNames), typeInfo, typeName)

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

-- | Returns a string representation of a value of type 'a'.
toString :: (Entity a) => a -> String
toString x = typeName (typeInfo x) ++ " " ++ unwords mappedRow
  where
    mappedRow = map fromSql (toRow x)

```