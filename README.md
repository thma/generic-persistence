# GenericPersistence - A Haskell Persistence Layer using Generics

[![Actions Status](https://github.com/thma/generic-persistence/workflows/Haskell%20CI/badge.svg)](https://github.com/thma/generic-persistence/actions)

![GP Logo](https://github.com/thma/generic-persistence/blob/main/gp-logo-300.png?raw=true)

## Introduction

GenericPersistence is a minimalistic Haskell persistence layer for relational databases. 
The approach relies on [GHC.Generics](https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-Generics.html). The actual database access is provided by the [HDBC](https://hackage.haskell.org/package/HDBC) library.

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

Several things are still missing:

- A query language
- Handling of nested transactions
- Handling auto-incrementing primary keys
- caching
- ...

## Available on Hackage

[https://hackage.haskell.org/package/generic-persistence](https://hackage.haskell.org/package/generic-persistence)

Add the following to your `package.yaml` file:

```yaml
dependencies:
- generic-persistence
```

## Short demo

Here now follows a short demo that shows how the library looks and feels from the user's point of view.

```haskell
-- allows automatic derivation from Entity type class
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import           Database.GP         
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           GHC.Generics

-- | An Entity data type with several fields, using record syntax.
data Person = Person
  { personID :: Int,
    name     :: String,
    age      :: Int,
    address  :: String
  }
  deriving (Generic, Entity, Show) -- deriving Entity allows us to use the GenericPersistence API

data Book = Book
  { book_id :: Int,
    title   :: String,
    author  :: String,
    year    :: Int
  }
  deriving (Generic, Show) -- no auto deriving of Entity, so we have to manually implement the Entity type class:

instance Entity Book where
  -- this is the primary key field of the Book data type
  idField = "book_id"

  -- this defines the mapping between the field names of the Book data type and the column names of the database table
  fieldsToColumns = [("book_id", "bookId"), ("title", "bookTitle"), ("author", "bookAuthor"), ("year", "bookYear")]

  -- this is the name of the database table
  tableName = "BOOK_TBL"

  -- this is the function that converts a row from the database table into a Book data type
  fromRow _conn row = return $ Book (col 0) (col 1) (col 2) (col 3)
    where
      col i = fromSql (row !! i)

  -- this is the function that converts a Book data type into a row for the database table
  toRow _conn b = return [toSql (book_id b), toSql (title b), toSql (author b), toSql (year b)]

main :: IO ()
main = do
  -- connect to a database
  conn <- ConnWrapper <$> connectSqlite3 ":memory:"

  -- initialize Person and Book tables
  _ <- setupTableFor conn :: IO Person
  _ <- setupTableFor conn :: IO Book

  let alice = Person 123456 "Alice" 25 "123 Main St"
      book = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937

  -- insert a Person into the database (persist will either insert or update)
  persist conn alice

  -- insert a second Person
  persist conn alice {personID = 123457, name = "Bob"}

  -- update a Person
  persist conn alice {address = "Elmstreet 1"}

  -- select a Person from a database
  alice' <- retrieveById conn (123456 :: Int) :: IO (Maybe Person)
  print alice'

  -- select all Persons from the database
  allPersons <- retrieveAll conn :: IO [Person]
  print allPersons

  -- delete a Person
  delete conn alice

  -- select all Persons from a database. The deleted Person is not in the result.
  allPersons' <- retrieveAll conn :: IO [Person]
  print allPersons'

  let book2 = Book {book_id = 2, title = "The Lord of the Ring", author = "J.R.R. Tolkien", year = 1954}

  -- this time we are using insert directly
  insert conn book
  insert conn book2
  allBooks <- retrieveAll conn :: IO [Book]
  print allBooks

  -- explicitly updating a Book
  update conn book2 {title = "The Lord of the Rings"}
  delete conn book

  allBooks' <- retrieveAll conn :: IO [Book]
  print allBooks'

  -- close connection
  disconnect conn
```

## Handling enumeration fields

Say we have a data type `Book` with an enumeration field of type `BookCategory`:

```haskell
data Book = Book
  { bookID :: Int,
    title   :: String,
    author  :: String,
    year    :: Int,
    category :: BookCategory
  }
  deriving (Generic, Entity, Show)

data BookCategory = Fiction | Travel | Arts | Science | History | Biography | Other
  deriving (Generic, Show, Enum)
```

In this case everything works out of the box, because *GenericPersistence* provides `Convertible` instances for all `Enum` types. `Convertible` instances are used to convert between Haskell types and database types.

If you do not want to use `Enum` types for your enumeration fields, you have to implement `Convertible` instances manually:

```haskell
data BookCategory = Fiction | Travel | Arts | Science | History | Biography | Other
  deriving (Generic, Show, Read)

instance Convertible BookCategory SqlValue where
  safeConvert = Right . toSql . show
  
instance Convertible SqlValue BookCategory where
  safeConvert = Right . read . fromSql  
```

## Handling embedded Objects

Say we have a data type `Article` with a field of type `Author`:

```haskell
data Article = Article
  { articleID :: Int,
    title     :: String,
    author    :: Author,
    year      :: Int
  }
  deriving (Generic, Show, Eq)

data Author = Author
  { authorID :: Int,
    name     :: String,
    address  :: String
  }
  deriving (Generic, Show, Eq)  
```

If we don't want to store the `Author` as a separate table, we can use the following approach to embed the `Author` into the `Article` table:

```haskell
instance Entity Article where
  -- in the fields to column mapping we specify that all fields of the 
  -- Author type are also mapped to columns of the Article table:
  fieldsToColumns :: [(String, String)]
  fieldsToColumns = [("articleID", "articleID"),
                       ("title", "title"), 
                       ("authorID", "authorID"), 
                       ("authorName", "authorName"), 
                       ("authorAddress", "authorAddress"),
                       ("year", "year")
                    ]

  -- in fromRow we have to manually construct the Author object from the 
  -- respective columns of the Article table and insert it 
  -- into the Article object:
  fromRow _conn row = return $ Article (col 0) (col 1) author (col 5)
    where
      col i = fromSql (row !! i)
      author = Author (col 2) (col 3) (col 4)

  -- in toRow we have to manually extract the fields of the Author object
  -- and insert them into the respective columns of the Article table:
  toRow _conn a = return [toSql (articleID a), toSql (title a), toSql authID, toSql authorName, toSql authorAddress, toSql (year a)]
    where 
      authID = authorID (author a)
      authorName = name (author a)
      authorAddress = address (author a)
```

## Handling 1:1 references

If we have the same data types as in the previous example, but we want to store the `Author` in a separate table, we can use the following approach:

```haskell
data Article = Article
  { articleID :: Int,
    title     :: String,
    author    :: Author,
    year      :: Int
  }
  deriving (Generic, Show, Eq)

data Author = Author
  { authorID :: Int,
    name     :: String,
    address  :: String
  }
  deriving (Generic, Entity, Show, Eq)

instance Entity Article where
  fieldsToColumns :: [(String, String)]
  fieldsToColumns =
    [ ("articleID", "articleID"),
      ("title", "title"),
      ("authorID", "authorID"),
      ("year", "year")
    ]

  fromRow :: Conn -> [SqlValue] -> IO Article
  fromRow conn row = do
    -- load author by foreign key
    authorById <- fromJust <$> retrieveById conn (row !! 2)
    -- insert author into article
    return $ rawArticle {author = authorById}
    where
      -- create article from row, with dummy author
      rawArticle = Article (col 0) (col 1) (Author (col 2) "" "") (col 3)
        where
          col i = fromSql (row !! i)

  toRow :: Conn -> Article -> IO [SqlValue]
  toRow conn a = do
    -- persist author first
    persist conn (author a)
    -- return row for article table where authorID is foreign key to author table 
    return [toSql (articleID a), toSql (title a), 
            toSql $ authorID (author a), toSql (year a)]
```
## Handling 1:n references

Now let's change the previous example by having a list of Article`s in the `Author` type:

```haskell
data Author = Author
  { authorID :: Int,
    name     :: String,
    address  :: String,
    articles :: [Article]
  }
  deriving (Generic, Show, Eq)

data Article = Article
  { articleID :: Int,
    title     :: String,
    authorId  :: Int,
    year      :: Int
  }
  deriving (Generic, Entity, Show, Eq)
```

So now we have a `1:n` relationship between `Author` and `Article`. 

We can handle this situation by using the following instance declaration for `Author`:

```haskell
i
```

## Todo

- coding free support for 1:1 and 1:n relationships (using more generics magic)


