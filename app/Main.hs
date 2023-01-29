{-# LANGUAGE DeriveAnyClass     #-}  -- allows automatic derivation from Entity type class
module Main (main, main1) where

import           Data.Data             (Data)
import           Database.HDBC         
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           GenericPersistence    (delete, persist, retrieveAll, retrieveById, Entity(..), setupTableFor) 
import           Entity


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
  fieldsToColumns _ = [("book_id", "bookId"), ("title", "bookTitle"), ("author", "bookAuthor"), ("year", "bookYear")]
  tableName _ = "BOOK_TBL"
  --fromRow :: [SqlValue] -> Book
  fromRow conn rc row = return $ Book (col 0) (col 1) (col 2) (col 3)
    where
      col i = fromSql (row !! i)

  --toRow :: Book -> [SqlValue]
  toRow conn rc b = return $ [toSql (book_id b), toSql (title b), toSql (author b), toSql (year b)]

main :: IO ()
main = do
  -- connect to a database
  conn <- connectSqlite3 "sqlite.db"

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
  alice' <- retrieveById conn emptyCache "123456" :: IO Person

  -- delete a Person from a database
  delete conn alice'

  -- close connection
  disconnect conn



main1 :: IO ()
main1 = do
  -- initialize Person table
  conn <- connectSqlite3 "sqlite.db"
  _ <- setupTableFor conn :: IO Person
  _ <- setupTableFor conn :: IO Book

  print "OK"

  let alice = Person 123456 "Alice" 25 "123 Main St"
      book  = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937

  

  -- insert a Person into a database
  persist conn alice

  -- insert a second Person in a database
  persist conn alice {personID = 123457, name = "Bob"}

  -- update a Person
  persist conn alice {address = "Elmstreet 1"}

  -- select a Person from a database
  alice' <- retrieveById conn emptyCache (123456 :: Int) :: IO Person
  print alice'

  -- select all Persons from a database
  allPersons <- retrieveAll conn emptyCache :: IO [Person]
  print allPersons

  -- delete a Person from a database
  delete conn alice

  -- select all Persons from a database
  allPersons' <- retrieveAll conn emptyCache :: IO [Person]
  print allPersons'

 

  let book2 = Book {book_id = 2, title = "The Lord of the Ring", author = "J.R.R. Tolkien", year = 1954}

  persist conn book
  persist conn book2
  allBooks <- retrieveAll conn emptyCache :: IO [Book]
  print allBooks

  persist conn book2 {title = "The Lord of the Rings"}
  delete conn book

  allBooks' <- retrieveAll conn emptyCache :: IO [Book]
  print allBooks'

  -- close connection
  disconnect conn

