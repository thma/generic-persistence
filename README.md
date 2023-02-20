# GenericPersistence - A Haskell persistence layer using Generics

[![Actions Status](https://github.com/thma/generic-persistence/workflows/Haskell%20CI/badge.svg)](https://github.com/thma/generic-persistence/actions)

![GP Logo](https://github.com/thma/generic-persistence/blob/main/gp-logo-300.png?raw=true)

## Introduction

GenericPersistence is a minimalistic Haskell persistence layer (on top of HDBC). 
The approach relies on Generics (`GHC.Generics`).

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
  deriving (Generic, Data, Entity, Show) -- deriving Entity allows to handle the type with GenericPersistence

data Book = Book
  { book_id :: Int,
    title   :: String,
    author  :: String,
    year    :: Int
  }
  deriving (Generic, Data, Show) -- no auto deriving of Entity, so we have to implement the Entity type class:

instance Entity Book where
  -- this is the primary key field of the Book data type
  idField _ = "book_id"

  -- this defines the mapping between the field names of the Book data type and the column names of the database table
  fieldsToColumns _ = [("book_id", "bookId"), ("title", "bookTitle"), ("author", "bookAuthor"), ("year", "bookYear")]

  -- this is the name of the database table
  tableName _ = "BOOK_TBL"

  -- this is the function that converts a row from the database table into a Book data type
  fromRow _c row = return $ Book (col 0) (col 1) (col 2) (col 3)
    where
      col i = fromSql (row !! i)

  -- this is the function that converts a Book data type into a row for the database table
  toRow _c b = return [toSql (book_id b), toSql (title b), toSql (author b), toSql (year b)]

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
  deriving (Generic, Data, Entity, Show)

data BookCategory = Fiction | Travel | Arts | Science | History | Biography | Other
  deriving (Generic, Data, Show, Enum)
```

In this case the everything works out of the box, because *GenericPersistence* provides `Convertible` instances for all `Enum` types.

If you do not want to use `Enum` types for your enumeration fields, you can implement `Convertible` instances manually:

```haskell
data BookCategory = Fiction | Travel | Arts | Science | History | Biography | Other
  deriving (Generic, Data, Show, Read)

instance Convertible BookCategory SqlValue where
  safeConvert = Right . toSql . show
  
instance Convertible SqlValue BookCategory where
  safeConvert = Right . read . fromSql  
```

## Handling embedded Objects

Say we have a data type `Article` with an field of type `Author`:

```haskell
data Article = Article
  { articleID :: Int,
    title     :: String,
    author    :: Author,
    year      :: Int
  }
  deriving (Generic, Data, Show, Eq)

data Author = Author
  { authorID :: Int,
    name     :: String,
    address  :: String
  }
  deriving (Generic, Data, Show, Eq)  
```

If we don't want to store the `Author` as a separate table, we can use the following approach to embed the `Author` into the `Article` table:

```haskell
instance Entity Article where
  -- in the fields to column mapping we specify that all fields of the 
  -- Author type are also mapped to columns of the Article table:
  fieldsToColumns :: Article -> [(String, String)]
  fieldsToColumns _ = [("articleID", "articleID"),
                       ("title", "title"), 
                       ("authorID", "authorID"), 
                       ("authorName", "authorName"), 
                       ("authorAddress", "authorAddress"),
                       ("year", "year")
                      ]

  -- in fromRow we have to manually construct the Author object from the 
  -- respective columns of the Article table and insert it 
  -- into the Article object:
  fromRow row = return $ Article (col 0) (col 1) author (col 5)
    where
      col i = fromSql (row !! i)
      author = Author (col 2) (col 3) (col 4)

  -- in toRow we have to manually extract the fields of the Author object
  -- and insert them into the respective columns of the Article table:
  toRow  a = return [toSql (articleID a), toSql (title a), toSql authID, toSql authorName, toSql authorAddress, toSql (year a)]
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
  deriving (Data, Show, Eq)

data Author = Author
  { authorID :: Int,
    name     :: String,
    address  :: String
  }
  deriving (Data, Entity, Show, Eq)  -- we derive Entity for Author

instance Entity Article where
  -- in the fields to column mapping we specify an additional authorID field 
  -- that will be used to store the id of the referenced Author object:
  fieldsToColumns :: Article -> [(String, String)]
  fieldsToColumns _ = [("articleID", "articleID"),
                       ("title", "title"), 
                       ("authorID", "authorID"),
                       ("year", "year")
                      ]

  -- in fromRow we have to manually retrieve the Author object from the 
  -- database (by using authorID as a foreign key)
  fromRow row = do
    maybeAuthor <- retrieveById (row !! 2) :: GP (Maybe Author)
    let author = fromJust maybeAuthor
    pure $ Article (col 0) (col 1) author (col 3)
    where
      col i = fromSql (row !! i)

  -- in toRow we have manually persist the Author object and include
  -- the authorID of the Author object in the Article row:    
  toRow a = do 
    persist (author a)
    return [toSql (articleID a), toSql (title a), toSql $ authorID (author a), toSql (year a)]

```
## Handling 1:n references

Now let's extend the previous example by also having a list of Àrticle`s in the `Author` type:

```haskell
data Article = Article
  { articleID :: Int,
    title     :: String,
    author    :: Author,
    year      :: Int
  }
  deriving (Data, Show, Eq)

data Author = Author
  { authorID :: Int,
    name     :: String,
    address  :: String,
    articles :: [Article]
  }
  deriving (Data, Show, Eq)  
```

So now we have a 1:n relationship between `Author` and `Article`. And in addtion we have the 1:1 relationship between `Article` and `Author` that we have seen in the previous example.

This situation is a bit more complicated, as we have to handle relationships between `Article` and `Author` at the same time. And we have to make sure that we don't end up in an infinite loop when we persist an `Author` object that contains a list of `Article` objects that in turn contain the same `Author` object. 

The same problem occurs when we retrieve an `Author` object that contains a list of `Article` objects that in turn contain the same `Author` object.

We can handle this situation by using the following approach:

```haskell
instance Entity Article where
  -- in the fields to column mapping we specify an additional authorID field:
  fieldsToColumns :: Article -> [(String, String)]
  fieldsToColumns _ = [("articleID", "articleID"),
                       ("title", "title"), 
                       ("authorID", "authorID"),
                       ("year", "year")
                      ]

  -- in fromRow we have to take care that we don't end up in an infinite loop
  -- so we first place a dummy Article object into the cache and then
  -- retrieve the Author object either from cache or from the db:
  fromRow :: [SqlValue] -> GP Article
  fromRow row = local (extendCtxCache rawArticle) $ do
    maybeAuthor <- getElseRetrieve (entityId rawAuthor)
    let author = fromJust maybeAuthor
    pure $ Article (col 0) (col 1) author (col 3)
    where
      col i = fromSql (row !! i)
      rawAuthor = (evidence :: Author) {authorID = col 2}
      rawArticle = Article (col 0) (col 1) rawAuthor (col 3)
    
  toRow a = do 
    persist (author a)
    return [toSql (articleID a), toSql (title a), toSql $ authorID (author a), toSql (year a)]


instance Entity Author where
  -- in the fields to column mapping we have anything for the articles field:
  fieldsToColumns :: Author -> [(String, String)]
  fieldsToColumns _ = [("authorID", "authorID"),
                       ("name", "name"), 
                       ("address", "address")
                      ]

  -- in fromRow we have to take care that we don't end up in an infinite loop.
  -- So we first place a dummy Author object into the cache and then
  -- retrieve matching list of  Article objects (from cache or from the db):
  fromRow :: [SqlValue] -> GP Author
  fromRow row = local (extendCtxCache rawAuthor) $ do
    articlesByAuth <- retrieveAllWhere (idField rawAuthor) (idValue rawAuthor) :: GP [Article]
    pure $ rawAuthor {articles= articlesByAuth}
    where
      col i = fromSql (row !! i)
      rawAuthor = Author (col 0) (col 1) (col 2) []

  -- in toRow we do not safe the articles field to avoid infinite loops:
  toRow :: Author -> GP [SqlValue]
  toRow a = do 
    return [toSql (authorID a), toSql (name a), toSql (address a)]
```

## Todo

- coding free support for 1:1 and 1:n relationships (using more generics magic)


