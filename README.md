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

## Handling embedded Objects

## Handling enumeration fields

## Handling 1:1 references

## Handling 1:n references

## The Entity type class

[Entity](src/Entity.hs)

## Todo

- test cases for embedded 
- testcases for enums
- testcases for 1.1
- testcases for 1:n
- How to hide the resolution cache from the user api?
- resolution cache with proper Map
- introduce nested module names like Database.GP.Entity
