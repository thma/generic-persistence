-- allows automatic derivation from Entity type class
{-# LANGUAGE DeriveAnyClass #-}

module Main (main, main1) where

import           Database.GP           (Entity (..), delete, insert, persist,
                                        retrieveAll, retrieveById,
                                        setupTableFor, update)
import           Database.HDBC         (ConnWrapper (..),
                                        IConnection (disconnect), fromSql,
                                        toSql)
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           GHC.Generics

-- | An Entity data type with several fields, using record syntax.
data Person = Person
  { personID :: Int,
    name     :: String,
    age      :: Int,
    address  :: String
  }
  deriving (Generic, Entity, Show) -- deriving Entity allows to handle the type with GenericPersistence

data Book = Book
  { book_id :: Int,
    title   :: String,
    author  :: String,
    year    :: Int
  }
  deriving (Generic, Show) -- no auto deriving of Entity, so we have to implement the Entity type class:

instance Entity Book where
  -- this is the primary key field of the Book data type
  idField = "book_id"

  -- this defines the mapping between the field names of the Book data type and the column names of the database table
  fieldsToColumns = [("book_id", "bookId"), ("title", "bookTitle"), ("author", "bookAuthor"), ("year", "bookYear")]

  -- this is the name of the database table
  tableName = "BOOK_TBL"

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

main1 :: IO ()
main1 = do
  -- connect to a database
  conn <- ConnWrapper <$> connectSqlite3 "sqlite.db"

  -- initialize Person table
  _ <- setupTableFor conn :: IO Person

  -- create a Person entity
  let alice = Person {personID = 123456, name = "Alice", age = 25, address = "Elmstreet 1"}

  -- insert a Person into a database
  persist conn alice

  -- update a Person
  persist conn alice {address = "Main Street 200"}

  -- select a Person from a database
  -- The result type must be provided explicitly, as `retrieveEntityById` has a polymorphic return type `IO a`.
  alice' <- retrieveById conn "123456" :: IO (Maybe Person)
  print alice'

  alice'' <- retrieveById conn "123456" :: IO (Maybe Person)

  print alice''

  -- delete a Person from a database
  delete conn alice

  -- close connection
  disconnect conn
