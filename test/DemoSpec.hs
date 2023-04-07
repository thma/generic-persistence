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
-- not needed for automatic spec discovery. 
-- (start up stack repl --test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

prepareDB :: IO Conn
prepareDB = do
  conn <- connect SQLite <$> connectSqlite3 ":memory:"
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

nameField :: Field
nameField = field "name"
ageField :: Field
ageField = field "age"
addressField :: Field
addressField = field "address"

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
  fieldsToColumns = [("book_id", "bookId"), ("title", "bookTitle"), ("author", "bookAuthor"), 
                     ("year", "bookYear"), ("category", "bookCategory")]
  tableName = "BOOK_TBL"
  fromRow _c row = pure $ Book (col 0) (col 1) (col 2) (col 3) (col 4)
    where
      col i = fromSql (row !! i)

  toRow _c b = pure [toSql (book_id b), toSql (title b), toSql (author b), toSql (year b), toSql (category b)]

person :: Person
person = Person 123456 "Alice" 25 "123 Main St"

manyPersons :: [Person]
manyPersons =
  [ Person 1 "Alice" 25 "123 Main St",
    Person 2 "Bob" 30 "456 Elm St",
    Person 3 "Charlie" 35 "789 Pine St",
    Person 4 "Dave" 40 "1011 Oak St",
    Person 5 "Eve" 45 "1213 Maple St",
    Person 6 "Frank" 50 "1415 Walnut St"
  ]


book :: Book
book = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937 Fiction

lower :: Field -> Field
lower = sqlFun "LOWER";

upper :: Field -> Field
upper = sqlFun "UPPER";

spec :: Spec
spec = do
  describe "GenericPersistence" $ do
    it "retrieves Entities using Generics" $ do
      conn <- prepareDB
      let bob = Person 1 "Bob" 36 "7 West Street"
      runRaw conn "INSERT INTO Person (personID, name, age, address) VALUES (1, \"Bob\", 36, \"7 West Street\");"
      runRaw conn "INSERT INTO Person (personID, name, age, address) VALUES (2, \"Alice\", 25, \"7 West Street\");"
      runRaw conn "INSERT INTO Person (personID, name, age, address) VALUES (3, \"Frank\", 56, \"7 West Street\");"
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 3
      head allPersons `shouldBe` bob
      person' <- selectById conn (1 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Just bob
    it "retrieves Entities using user implementation" $ do
      conn <- prepareDB
      let hobbit = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937 Fiction
      runRaw conn "INSERT INTO BOOK_TBL (bookId, bookTitle, bookAuthor, bookYear, bookCategory) VALUES (1, \"The Hobbit\", \"J.R.R. Tolkien\", 1937, 0);"
      runRaw conn "INSERT INTO BOOK_TBL (bookId, bookTitle, bookAuthor, bookYear, bookCategory) VALUES (2, \"The Lord of the Rings\", \"J.R.R. Tolkien\", 1955, 0);"
      runRaw conn "INSERT INTO BOOK_TBL (bookId, bookTitle, bookAuthor, bookYear, bookCategory) VALUES (3, \"Smith of Wootton Major\", \"J.R.R. Tolkien\", 1967, 0);"
      allBooks <- select conn allEntries :: IO [Book]
      length allBooks `shouldBe` 3
      head allBooks `shouldBe` hobbit
      book' <- selectById conn (1 :: Int) :: IO (Maybe Book)
      book' `shouldBe` Just hobbit
    it "retrieves Entities using a simple Query DSL" $ do
      conn <- prepareDB
      let bob = Person 1 "Bob" 36 "West Street 79"
          alice = Person 2 "Alice" 25 "West Street 90"
          charlie = Person 3 "Charlie" 35 "West Street 40"
      insertMany conn [alice, bob, charlie]
      one <- select conn (nameField =. "Bob" &&. ageField =. (36 :: Int))
      length one `shouldBe` 1
      head one `shouldBe` bob
      two <- select conn (nameField =. "Bob" ||. ageField =. (25 :: Int))
      length two `shouldBe` 2
      two `shouldContain` [bob, alice]
      three <- select conn (addressField `like` "West Street %") :: IO [Person]
      length three `shouldBe` 3
      empty <- select conn (not' $ addressField `like` "West Street %") :: IO [Person]
      length empty `shouldBe` 0
      boomers <- select conn (ageField >. (30 :: Int))
      length boomers `shouldBe` 2
      boomers `shouldContain` [bob, charlie]
      thirtySomethings <- select conn (ageField `between` (30 :: Int, 40 :: Int)) :: IO [Person]
      length thirtySomethings `shouldBe` 2
      thirtySomethings `shouldContain` [bob, charlie]
      aliceAndCharlie <- select conn (nameField `in'` ["Alice", "Charlie"])
      length aliceAndCharlie `shouldBe` 2
      aliceAndCharlie `shouldContain` [alice, charlie]
      noOne <- select conn (isNull nameField) :: IO [Person]
      length noOne `shouldBe` 0
      allPersons <- select conn (not' $ isNull nameField) :: IO [Person]
      length allPersons `shouldBe` 3
      peopleFromWestStreet <- select conn (lower(upper addressField)) `like` "west street %") :: IO [Person]
      length peopleFromWestStreet `shouldBe` 3
      charlie' <- select conn (byId "3") :: IO [Person]
      length charlie' `shouldBe` 1
      head charlie' `shouldBe` charlie
      
    it "persists new Entities using Generics" $ do
      conn <- prepareDB
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 0
      persist conn person
      allPersons' <- select conn allEntries :: IO [Person]
      length allPersons' `shouldBe` 1
      person' <- selectById conn (123456 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Just person
    it "persists new Entities using user implementation" $ do
      conn <- prepareDB
      allbooks <- select conn allEntries :: IO [Book]
      length allbooks `shouldBe` 0
      persist conn book
      allbooks' <- select conn allEntries :: IO [Book]
      length allbooks' `shouldBe` 1
      book' <- selectById conn (1 :: Int) :: IO (Maybe Book)
      book' `shouldBe` Just book
    it "persists existing Entities using Generics" $ do
      conn <- prepareDB
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 0
      persist conn person
      allPersons' <- select conn allEntries :: IO [Person]
      length allPersons' `shouldBe` 1
      persist conn person {age = 26}
      person' <- selectById conn (123456 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Just person {age = 26}
    it "persists existing Entities using user implementation" $ do
      conn <- prepareDB
      allbooks <- select conn allEntries :: IO [Book]
      length allbooks `shouldBe` 0
      persist conn book
      allbooks' <- select conn allEntries :: IO [Book]
      length allbooks' `shouldBe` 1
      persist conn book {year = 1938}
      book' <- selectById conn (1 :: Int) :: IO (Maybe Book)
      book' `shouldBe` Just book {year = 1938}
    it "inserts Entities using Generics" $ do
      conn <- prepareDB
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 0
      insert conn person
      allPersons' <- select conn allEntries :: IO [Person]
      length allPersons' `shouldBe` 1
      person' <- selectById conn (123456 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Just person
    it "inserts many Entities re-using a single prepared stmt" $ do
      conn <- prepareDB
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 0
      insertMany conn manyPersons
      allPersons' <- select conn allEntries :: IO [Person]
      length allPersons' `shouldBe` 6
    it "updates many Entities re-using a single prepared stmt" $ do
      conn <- prepareDB
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 0
      insertMany conn manyPersons
      allPersons' <- select conn allEntries :: IO [Person]
      length allPersons' `shouldBe` 6
      let manyPersons' = map (\p -> p {name = "Bob"}) manyPersons
      updateMany conn manyPersons'
      allPersons'' <- select conn allEntries :: IO [Person]
      all (\p -> name p == "Bob") allPersons'' `shouldBe` True
    it "deletes many Entities re-using a single prepared stmt" $ do
      conn <- prepareDB
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 0
      insertMany conn manyPersons
      allPersons' <- select conn allEntries :: IO [Person]
      length allPersons' `shouldBe` 6   
      deleteMany conn allPersons'
      allPersons'' <- select conn allEntries :: IO [Person]
      length allPersons'' `shouldBe` 0

    it "inserts Entities using user implementation" $ do
      conn <- prepareDB
      allbooks <- select conn allEntries :: IO [Book]
      length allbooks `shouldBe` 0
      insert conn book
      allbooks' <- select conn allEntries :: IO [Book]
      length allbooks' `shouldBe` 1
      book' <- selectById conn (1 :: Int) :: IO (Maybe Book)
      book' `shouldBe` Just book
    it "updates Entities using Generics" $ do
      conn <- prepareDB
      insert conn person
      update conn person {name = "Bob"}
      person' <- selectById conn (123456 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Just person {name = "Bob"}
    it "updates Entities using user implementation" $ do
      conn <- prepareDB
      insert conn book
      update conn book {title = "The Lord of the Rings"}
      book' <- selectById conn (1 :: Int) :: IO (Maybe Book)
      book' `shouldBe` Just book {title = "The Lord of the Rings"}
    it "deletes Entities using Generics" $ do
      conn <- prepareDB
      insert conn person
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 1
      delete conn person
      allPersons' <- select conn allEntries :: IO [Person]
      length allPersons' `shouldBe` 0
    it "deletes Entities using user implementation" $ do
      conn <- prepareDB
      insert conn book
      allBooks <- select conn allEntries :: IO [Book]
      length allBooks `shouldBe` 1
      delete conn book
      allBooks' <- select conn allEntries :: IO [Book]
      length allBooks' `shouldBe` 0
