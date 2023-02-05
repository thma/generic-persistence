{-# LANGUAGE DeriveAnyClass     #-}  -- allows automatic derivation from Entity type class
module GenericPersistenceSpec
  ( test
  , spec
  ) where


import           Test.Hspec
import           Data.Data             
import           Database.HDBC         
import           Database.HDBC.Sqlite3
import           GenericPersistence



-- `test` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery. (start up stack repl --test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

prepareDatabase :: IO Connection
prepareDatabase = do
  conn <- connectSqlite3 ":memory:"
  _ <- setupTableFor conn :: IO Person
  runRaw conn "DROP TABLE IF EXISTS BOOK_TBL;"
  runRaw conn "CREATE TABLE BOOK_TBL (bookId INT PRIMARY KEY, bookTitle TEXT, bookAuthor TEXT, bookYear INT, bookCategory INT);"
  --_ <- setupTableFor conn :: IO Book
  return conn

-- | A data type with several fields, using record syntax.
data Person = Person
  { personID :: Int,
    name     :: String,
    age      :: Int,
    address  :: String
  }
  deriving (Data, Entity, Show, Eq)

data Book = Book
  { book_id :: Int,
    title   :: String,
    author  :: String,
    year    :: Int,
    category :: BookCategory
  }
  deriving (Data, Show, Eq)

data BookCategory = Fiction | Travel | Arts | Science | History | Biography | Other
  deriving (Data, Read, Show, Eq, Enum)





  
instance Entity Book where
  idField _ = "book_id"
  fieldsToColumns _ = [("book_id", "bookId"), ("title", "bookTitle"), ("author", "bookAuthor"), ("year", "bookYear"), ("category", "bookCategory")]
  tableName _ = "BOOK_TBL"
  fromRow _ _ row = pure $ Book (col 0) (col 1) (col 2) (col 3) (col 4)
    where
      col i = fromSql (row !! i)
  toRow _ _ b = pure [toSql (book_id b), toSql (title b), toSql (author b), toSql (year b), toSql (category b)]


person :: Person
person = Person 123456 "Alice" 25 "123 Main St"

book :: Book
book = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937 Fiction


spec :: Spec
spec = do
  describe "GenericPersistence" $ do
    it "retrieves Entities using Generics" $ do
      conn <- prepareDatabase
      let bob = Person 1 "Bob" 36 "7 West Street"
      runRaw conn "INSERT INTO Person (personID, name, age, address) VALUES (1, \"Bob\", 36, \"7 West Street\");"
      runRaw conn "INSERT INTO Person (personID, name, age, address) VALUES (2, \"Alice\", 25, \"7 West Street\");"
      runRaw conn "INSERT INTO Person (personID, name, age, address) VALUES (3, \"Frank\", 56, \"7 West Street\");"
      allPersons <- retrieveAll conn mempty :: IO [Person]
      length allPersons `shouldBe` 3
      head allPersons `shouldBe` bob
      person' <- retrieveById conn mempty (1 :: Int) :: IO (Maybe Person)
      disconnect conn
      person' `shouldBe` Just bob
    it "retrieves Entities using user implementation" $ do
      conn <- prepareDatabase
      let hobbit = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937 Fiction
      runRaw conn "INSERT INTO BOOK_TBL (bookId, bookTitle, bookAuthor, bookYear, bookCategory) VALUES (1, \"The Hobbit\", \"J.R.R. Tolkien\", 1937, 0);"
      runRaw conn "INSERT INTO BOOK_TBL (bookId, bookTitle, bookAuthor, bookYear, bookCategory) VALUES (2, \"The Lord of the Rings\", \"J.R.R. Tolkien\", 1955, 0);"
      runRaw conn "INSERT INTO BOOK_TBL (bookId, bookTitle, bookAuthor, bookYear, bookCategory) VALUES (3, \"Smith of Wootton Major\", \"J.R.R. Tolkien\", 1967, 0);"
      allBooks <- retrieveAll conn mempty :: IO [Book]
      length allBooks `shouldBe` 3
      head allBooks `shouldBe` hobbit
      book' <- retrieveById conn mempty (1 :: Int) :: IO (Maybe Book)
      disconnect conn
      book' `shouldBe` Just hobbit
    it "persists new Entities using Generics" $ do
      conn <- prepareDatabase
      allPersons <- retrieveAll conn mempty :: IO [Person]
      length allPersons `shouldBe` 0
      persist conn person
      allPersons' <- retrieveAll conn mempty :: IO [Person]
      length allPersons' `shouldBe` 1
      person' <- retrieveById conn mempty (123456 :: Int) :: IO (Maybe Person)
      disconnect conn
      person' `shouldBe` Just person
    it "persists new Entities using user implementation" $ do
      conn <- prepareDatabase
      allbooks <- retrieveAll conn mempty :: IO [Book]
      length allbooks `shouldBe` 0
      persist conn book
      allbooks' <- retrieveAll conn mempty :: IO [Book]
      length allbooks' `shouldBe` 1
      book' <- retrieveById conn mempty (1 :: Int) :: IO (Maybe Book)
      disconnect conn
      book' `shouldBe` Just book
    it "persists existing Entities using Generics" $ do
      conn <- prepareDatabase
      allPersons <- retrieveAll conn mempty :: IO [Person]
      length allPersons `shouldBe` 0
      persist conn person
      allPersons' <- retrieveAll conn mempty :: IO [Person]
      length allPersons' `shouldBe` 1
      persist conn person {age = 26}
      person' <- retrieveById conn mempty (123456 :: Int) :: IO (Maybe Person)
      disconnect conn
      person' `shouldBe` Just person {age = 26}
    it "persists existing Entities using user implementation" $ do
      conn <- prepareDatabase
      allbooks <- retrieveAll conn mempty :: IO [Book]
      length allbooks `shouldBe` 0
      persist conn book
      allbooks' <- retrieveAll conn mempty :: IO [Book]
      length allbooks' `shouldBe` 1
      persist conn book {year = 1938}
      book' <- retrieveById conn mempty (1 :: Int) :: IO (Maybe Book)
      disconnect conn
      book' `shouldBe` Just book {year = 1938}
    it "inserts Entities using Generics" $ do
      conn <- prepareDatabase
      allPersons <- retrieveAll conn mempty :: IO [Person]
      length allPersons `shouldBe` 0
      insert conn person
      allPersons' <- retrieveAll conn mempty :: IO [Person]
      length allPersons' `shouldBe` 1
      person' <- retrieveById conn mempty (123456 :: Int) :: IO (Maybe Person)
      disconnect conn
      person' `shouldBe` Just person
    it "inserts Entities using user implementation" $ do
      conn <- prepareDatabase
      allbooks <- retrieveAll conn mempty :: IO [Book]
      length allbooks `shouldBe` 0
      insert conn book
      allbooks' <- retrieveAll conn mempty :: IO [Book]
      length allbooks' `shouldBe` 1
      book' <- retrieveById conn mempty (1 :: Int) :: IO (Maybe Book)
      disconnect conn
      book' `shouldBe` Just book
    it "updates Entities using Generics" $ do
      conn <- prepareDatabase
      insert conn person
      update conn person {name = "Bob"}
      person' <- retrieveById conn mempty (123456 :: Int) :: IO (Maybe Person)
      disconnect conn
      person' `shouldBe` Just person {name = "Bob"}
    it "updates Entities using user implementation" $ do
      conn <- prepareDatabase
      insert conn book
      update conn book {title = "The Lord of the Rings"}
      book' <- retrieveById conn mempty (1 :: Int) :: IO (Maybe Book)
      disconnect conn
      book' `shouldBe` Just book {title = "The Lord of the Rings"}
    it "deletes Entities using Generics" $ do
      conn <- prepareDatabase
      insert conn person
      allPersons <- retrieveAll conn mempty :: IO [Person]
      length allPersons `shouldBe` 1
      delete conn person
      allPersons' <- retrieveAll conn mempty :: IO [Person]
      length allPersons' `shouldBe` 0
      disconnect conn
    it "deletes Entities using user implementation" $ do
      conn <- prepareDatabase
      insert conn book
      allBooks <- retrieveAll conn mempty :: IO [Book]
      length allBooks `shouldBe` 1
      delete conn book
      allBooks' <- retrieveAll conn mempty :: IO [Book]
      length allBooks' `shouldBe` 0
      disconnect conn
