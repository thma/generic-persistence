-- allows automatic derivation from Entity type class
{-# LANGUAGE DeriveAnyClass #-}

module GenericPersistenceSpec
  ( test,
    spec,
  )
where

import           Database.GP.GenericPersistence
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           GHC.Generics
import           Test.Hspec

-- `test` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery. (start up stack repl --test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

prepareDB :: IO Conn
prepareDB = do
  conn <- Conn SQLite <$> connectSqlite3 ":memory:"
  setupTableFor @Person conn
  setupTableFor @Book conn
  return conn

-- | A data type with several fields, using record syntax.
data Person = Person
  { personID :: Int,
    name     :: String,
    age      :: Int,
    address  :: String
  }
  deriving (Generic, Entity, Show, Eq)

data Book = Book
  { book_id  :: Int,
    title    :: String,
    author   :: String,
    year     :: Int,
    category :: BookCategory
  }
  deriving (Generic, Show, Eq)

data BookCategory = Fiction | Travel | Arts | Science | History | Biography | Other
  deriving (Generic, Read, Show, Eq, Enum)

instance Entity Book where
  idField = "book_id"
  fieldsToColumns = [("book_id", "bookId"), ("title", "bookTitle"), ("author", "bookAuthor"), ("year", "bookYear"), ("category", "bookCategory")]
  tableName = "BOOK_TBL"
  fromRow _c row = pure $ Book (col 0) (col 1) (col 2) (col 3) (col 4)
    where
      col i = fromSql (row !! i)

  toRow _c b = pure [toSql (book_id b), toSql (title b), toSql (author b), toSql (year b), toSql (category b)]

person :: Person
person = Person 123456 "Alice" 25 "123 Main St"

book :: Book
book = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937 Fiction

spec :: Spec
spec = do
  describe "GenericPersistence" $ do
    it "retrieves Entities using Generics" $ do
      conn <- prepareDB
      let bob = Person 1 "Bob" 36 "7 West Street"
      runRaw conn "INSERT INTO Person (personID, name, age, address) VALUES (1, \"Bob\", 36, \"7 West Street\");"
      runRaw conn "INSERT INTO Person (personID, name, age, address) VALUES (2, \"Alice\", 25, \"7 West Street\");"
      runRaw conn "INSERT INTO Person (personID, name, age, address) VALUES (3, \"Frank\", 56, \"7 West Street\");"
      allPersons <- retrieveAll conn :: IO [Person]
      length allPersons `shouldBe` 3
      head allPersons `shouldBe` bob
      person' <- retrieveById conn (1 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Just bob
    it "retrieves Entities using user implementation" $ do
      conn <- prepareDB
      let hobbit = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937 Fiction
      runRaw conn "INSERT INTO BOOK_TBL (bookId, bookTitle, bookAuthor, bookYear, bookCategory) VALUES (1, \"The Hobbit\", \"J.R.R. Tolkien\", 1937, 0);"
      runRaw conn "INSERT INTO BOOK_TBL (bookId, bookTitle, bookAuthor, bookYear, bookCategory) VALUES (2, \"The Lord of the Rings\", \"J.R.R. Tolkien\", 1955, 0);"
      runRaw conn "INSERT INTO BOOK_TBL (bookId, bookTitle, bookAuthor, bookYear, bookCategory) VALUES (3, \"Smith of Wootton Major\", \"J.R.R. Tolkien\", 1967, 0);"
      allBooks <- retrieveAll conn :: IO [Book]
      length allBooks `shouldBe` 3
      head allBooks `shouldBe` hobbit
      book' <- retrieveById conn (1 :: Int) :: IO (Maybe Book)
      book' `shouldBe` Just hobbit
    it "persists new Entities using Generics" $ do
      conn <- prepareDB
      allPersons <- retrieveAll conn :: IO [Person]
      length allPersons `shouldBe` 0
      persist conn person
      allPersons' <- retrieveAll conn :: IO [Person]
      length allPersons' `shouldBe` 1
      person' <- retrieveById conn (123456 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Just person
    it "persists new Entities using user implementation" $ do
      conn <- prepareDB
      allbooks <- retrieveAll conn :: IO [Book]
      length allbooks `shouldBe` 0
      persist conn book
      allbooks' <- retrieveAll conn :: IO [Book]
      length allbooks' `shouldBe` 1
      book' <- retrieveById conn (1 :: Int) :: IO (Maybe Book)
      book' `shouldBe` Just book
    it "persists existing Entities using Generics" $ do
      conn <- prepareDB
      allPersons <- retrieveAll conn :: IO [Person]
      length allPersons `shouldBe` 0
      persist conn person
      allPersons' <- retrieveAll conn :: IO [Person]
      length allPersons' `shouldBe` 1
      persist conn person {age = 26}
      person' <- retrieveById conn (123456 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Just person {age = 26}
    it "persists existing Entities using user implementation" $ do
      conn <- prepareDB
      allbooks <- retrieveAll conn :: IO [Book]
      length allbooks `shouldBe` 0
      persist conn book
      allbooks' <- retrieveAll conn :: IO [Book]
      length allbooks' `shouldBe` 1
      persist conn book {year = 1938}
      book' <- retrieveById conn (1 :: Int) :: IO (Maybe Book)
      book' `shouldBe` Just book {year = 1938}
    it "inserts Entities using Generics" $ do
      conn <- prepareDB
      allPersons <- retrieveAll conn :: IO [Person]
      length allPersons `shouldBe` 0
      insert conn person
      allPersons' <- retrieveAll conn :: IO [Person]
      length allPersons' `shouldBe` 1
      person' <- retrieveById conn (123456 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Just person
    it "inserts Entities using user implementation" $ do
      conn <- prepareDB
      allbooks <- retrieveAll conn :: IO [Book]
      length allbooks `shouldBe` 0
      insert conn book
      allbooks' <- retrieveAll conn :: IO [Book]
      length allbooks' `shouldBe` 1
      book' <- retrieveById conn (1 :: Int) :: IO (Maybe Book)
      book' `shouldBe` Just book
    it "updates Entities using Generics" $ do
      conn <- prepareDB
      insert conn person
      update conn person {name = "Bob"}
      person' <- retrieveById conn (123456 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Just person {name = "Bob"}
    it "updates Entities using user implementation" $ do
      conn <- prepareDB
      insert conn book
      update conn book {title = "The Lord of the Rings"}
      book' <- retrieveById conn (1 :: Int) :: IO (Maybe Book)
      book' `shouldBe` Just book {title = "The Lord of the Rings"}
    it "deletes Entities using Generics" $ do
      conn <- prepareDB
      insert conn person
      allPersons <- retrieveAll conn :: IO [Person]
      length allPersons `shouldBe` 1
      delete conn person
      allPersons' <- retrieveAll conn :: IO [Person]
      length allPersons' `shouldBe` 0
    it "deletes Entities using user implementation" $ do
      conn <- prepareDB
      insert conn book
      allBooks <- retrieveAll conn :: IO [Book]
      length allBooks `shouldBe` 1
      delete conn book
      allBooks' <- retrieveAll conn :: IO [Book]
      length allBooks' `shouldBe` 0
