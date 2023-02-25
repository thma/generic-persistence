-- allows automatic derivation from Entity type class
{-# LANGUAGE DeriveAnyClass #-}

module Main (main, main1, main2, main3) where

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

  -- -- this is the function that converts a row from the database table into a Book data type
  -- fromRow _c row = return $ Book (col 0) (col 1) (col 2) (col 3)
  --   where
  --     col i = fromSql (row !! i)

  -- -- this is the function that converts a Book data type into a row for the database table
  -- toRow _c b = return [toSql (book_id b), toSql (title b), toSql (author b), toSql (year b)]

main :: IO ()
main = do
  -- connect to a database
  conn <- connect SQLite <$> connectSqlite3 "sqlite.db"

  -- initialize Person table
  setupTableFor @Person conn

  -- create a Person entity
  let alice = Person {personID = 123456, name = "Alice", age = 25, address = "Elmstreet 1"}

  -- insert a Person into a database
  insert conn alice

  -- update a Person
  update conn alice {address = "Main Street 200"}

  -- select a Person from a database
  -- The result type must be provided by the call site, 
  -- as `retrieveEntityById` has a polymorphic return type `IO (Maybe a)`.
  alice' <- retrieveById @Person conn "123456" 
  print alice'

  -- select all Persons from a database
  allPersons <- retrieveAll @Person conn
  print allPersons

  -- delete a Person from a database
  delete conn alice

  -- select all Persons from a database. Now it should be empty.
  allPersons' <- retrieveAll conn :: IO [Person]
  print allPersons'

  -- close connection
  disconnect conn

main1 :: IO ()
main1 = do
  -- connect to a database
  conn <- Conn SQLite False <$> connectSqlite3 "test.db" -- ":memory:" 

  -- initialize Person and Book tables
  setupTableFor @Person conn
  setupTableFor @Book conn

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


main2 :: IO ()
main2 = do
  -- connect to a database
  conn <- connect SQLite <$> connectSqlite3 ":memory:" 

  -- initialize Person table
  setupTableFor @Person conn

  let alice = Person 1 "Alice" 25 "123 Main St"
      bob = Person 2 "Bob" 30 "456 Elm St"
      charlie = Person 3 "Charlie" 35 "789 Pine St"
      dave = Person 4 "Dave" 40 "1011 Oak St"
      eve = Person 5 "Eve" 45 "1213 Maple St"
      frank = Person 6 "Frank" 50 "1415 Walnut St"
      people = [alice, bob, charlie, dave, eve, frank]
      stmt = "SELECT * FROM Person WHERE age >= ?"

  -- insert all persons into the database
  insertMany conn people

  -- select all Person with age >= 40
  resultRows <- quickQuery conn stmt [toSql (40 :: Int)]
  fourtplussers <- entitiesFromRows @Person conn resultRows
  print fourtplussers
  
main3 :: IO ()
main3 = do
  -- connect to a database
  conn <- connect SQLite <$> connectSqlite3 "test.db" 

  -- initialize Person table
  setupTableFor @Person conn

  let alice = Person 1 "Alice" 25 "123 Main St"
      bob = Person 2 "Bob" 30 "456 Elm St"
      charlie = Person 3 "Charlie" 35 "789 Pine St"
      dave = Person 4 "Dave" 40 "1011 Oak St"
      eve = Person 5 "Eve" 45 "1213 Maple St"
      frank = Person 6 "Frank" 50 "1415 Walnut St"
      people = [alice, bob, charlie, dave, eve, frank]

  -- insert all persons into the database
  insertMany conn people  

  people' <- retrieveAll @Person conn
  print $ length people'


