{-# LANGUAGE DeriveAnyClass     #-}  -- allows automatic derivation from Entity type class
module Main (main, main1) where

--import           Data.Data             (Data)
import           Database.HDBC         
import           Database.HDBC.Sqlite3
import           GenericPersistence    
import           RIO


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

  fromRow row = return $ Book (col 0) (col 1) (col 2) (col 3)
    where
      col i = fromSql (row !! i)

  toRow b = return [toSql (book_id b), toSql (title b), toSql (author b), toSql (year b)]


main :: IO ()
main = do
  -- connect to a database
  conn <- connectSqlite3 "sqlite.db"
  let ctx = Ctx (ConnWrapper conn) mempty 

  runRIO ctx $ do 
    -- initialize Person table
    _ <- setupTableFor :: GP Person
    
    -- create a Person entity
    let alice = Person {personID = 123456, name = "Alice", age = 25, address = "Elmstreet 1"}
    
    -- insert a Person into a database
    persist alice
  
    -- update a Person
    persist alice {address = "Main Street 200"}
  
    -- select a Person from a database
    -- The result type must be provided explicitly, as `retrieveEntityById` has a polymorphic return type `IO a`.
    alice' <- retrieveById "123456" :: GP (Maybe Person)
    liftIO $ print alice'
  
    alice'' <- retrieveById "123456" :: GP (Maybe Person)
  
    liftIO $ print alice''
  
  -- delete a Person from a database
    delete alice
  
  -- close connection
  disconnect conn



main1 :: IO ()
main1 = do
  -- initialize Person table
  conn <- connectSqlite3 "sqlite.db"
  let ctx = Ctx (ConnWrapper conn) mempty 
  runRIO ctx $ do
    
    _ <- setupTableFor :: GP Person
    _ <- setupTableFor :: GP Book
  
  
    let alice = Person 123456 "Alice" 25 "123 Main St"
        book  = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937
  
    -- insert a Person into a database
    persist alice
  
    -- insert a second Person in a database
    persist alice {personID = 123457, name = "Bob"}
  
    -- update a Person
    persist alice {address = "Elmstreet 1"}
  
    -- select a Person from a database
    alice' <- retrieveById (123456 :: Int) :: GP (Maybe Person)
    liftIO $ print alice'
  
    -- select all Persons from a database
    allPersons <- retrieveAll :: GP [Person]
    liftIO $ print allPersons
  
    -- delete a Person from a database
    delete alice
  
    -- select all Persons from a database
    allPersons' <- retrieveAll :: GP [Person]
    liftIO $ print allPersons'
  
    let book2 = Book {book_id = 2, title = "The Lord of the Ring", author = "J.R.R. Tolkien", year = 1954}
  
    persist book
    persist book2
    allBooks <- retrieveAll :: GP [Book]
    liftIO $ print allBooks
  
    persist book2 {title = "The Lord of the Rings"}
    delete book
  
    allBooks' <- retrieveAll :: GP [Book]
    liftIO $ print allBooks'
  
  -- close connection  
  disconnect conn

