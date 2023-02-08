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
- Handling of nested transactions
- Handling auto-incrementing primary keys
- ...


## Short demo

Here now follows a short demo that shows how the library looks and feels from the user's point of view.

```haskell
-- allows automatic derivation from Entity type class
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import           Data.Data             (Data)
import           Database.GP           (Entity (..), GP, delete, insert, liftIO,
                                        persist, retrieveAll, retrieveById,
                                        runGP, setupTableFor, update)
import           Database.HDBC         (IConnection (disconnect), fromSql,
                                        toSql)
import           Database.HDBC.Sqlite3 (connectSqlite3)


-- | An Entity data type with several fields, using record syntax.
data Person = Person
  { personID :: Int,
    name     :: String,
    age      :: Int,
    address  :: String
  }
  deriving (Data, Entity, Show) -- deriving Entity allows to handle the type with GenericPersistence

data Book = Book
  { book_id :: Int,
    title   :: String,
    author  :: String,
    year    :: Int
  }
  deriving (Data, Show) -- no auto deriving of Entity, so we have to implement the Entity type class:

instance Entity Book where
  -- this is the primary key field of the Book data type
  idField _ = "book_id"

  -- this defines the mapping between the field names of the Book data type and the column names of the database table
  fieldsToColumns _ = [("book_id", "bookId"), ("title", "bookTitle"), ("author", "bookAuthor"), ("year", "bookYear")]

  -- this is the name of the database table
  tableName _ = "BOOK_TBL"

  -- this is the function that converts a row from the database table into a Book data type
  fromRow row = return $ Book (col 0) (col 1) (col 2) (col 3)
    where
      col i = fromSql (row !! i)

  -- this is the function that converts a Book data type into a row for the database table
  toRow b = return [toSql (book_id b), toSql (title b), toSql (author b), toSql (year b)]

main :: IO ()
main = do
  -- connect to a database
  conn <- connectSqlite3 "sqlite.db"
  -- take the connection and execute all persistence operations in the GP monad (type alias for RIO Ctx)
  runGP conn $ do
    _ <- setupTableFor :: GP Person
    _ <- setupTableFor :: GP Book

    let alice = Person 123456 "Alice" 25 "123 Main St"
        book = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937

    -- insert a Person into the database (persist will either insert or update)
    persist alice

    -- insert a second Person
    persist alice {personID = 123457, name = "Bob"}

    -- update a Person
    persist alice {address = "Elmstreet 1"}

    -- select a Person from a database
    alice' <- retrieveById (123456 :: Int) :: GP (Maybe Person)
    liftIO $ print alice'

    -- select all Persons from the database
    allPersons <- retrieveAll :: GP [Person]
    liftIO $ print allPersons

    -- delete a Person
    delete alice

    -- select all Persons from a database. The deleted Person is not in the result.
    allPersons' <- retrieveAll :: GP [Person]
    liftIO $ print allPersons'

    let book2 = Book {book_id = 2, title = "The Lord of the Ring", author = "J.R.R. Tolkien", year = 1954}

    -- this time we are using insert directly
    insert book
    insert book2
    allBooks <- retrieveAll :: GP [Book]
    liftIO $ print allBooks

    -- explicitly updating a Book
    update book2 {title = "The Lord of the Rings"}
    delete book

    allBooks' <- retrieveAll :: GP [Book]
    liftIO $ print allBooks'
```

## Handling enumeration fields



## Handling embedded Objects

## Handling 1:1 references

## Handling 1:n references


## Todo

- testcases for 1.1
- testcases for 1:n
- resolution cache with proper Map

