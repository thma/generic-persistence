{-# OPTIONS_GHC -Wno-deprecations #-}

module PostgresSpec
  ( test,
    spec,
  )
where

import           Control.Exception
import           Database.GP
import           Database.HDBC.PostgreSQL
import           GHC.Generics             hiding (Selector)
import           Test.Hspec

-- `test` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
-- (start up stack repl --test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

prepareDB :: IO Conn
prepareDB = do
  conn <- connect ExplicitCommit <$> connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin port=5431"
  setupTable @Person conn defaultPostgresMapping
  setupTable @Book conn defaultPostgresMapping
  setupTable @Boat conn defaultPostgresMapping
  setupTable @BearerToken conn defaultPostgresMapping
  _ <- run conn "DROP TABLE IF EXISTS Car;" []
  _ <- run conn "CREATE TABLE Car (carID serial4 PRIMARY KEY, carType varchar);" []
  commit conn
  return conn

-- | A data type with several fields, using record syntax.
data Person = Person
  { personID :: Int,
    name     :: String,
    age      :: Int,
    address  :: String
  }
  deriving (Generic, Show, Eq)

instance Entity Person "personID" Int where
  autoIncrement = False

data Car = Car
  { carID   :: Int,
    carType :: String
  }
  deriving (Generic, Show, Eq)

instance Entity Car "carID" Int

data Boat = Boat
  { boatID   :: Int,
    boatType :: String
  }
  deriving (Generic, Show, Eq)

instance Entity Boat "boatID" Int where
  autoIncrement = False

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

instance Entity Book "book_id" Int where
  fieldsToColumns =
    [ ("book_id", "bookId"),
      ("title", "bookTitle"),
      ("author", "bookAuthor"),
      ("year", "bookYear"),
      ("category", "bookCategory")
    ]
  tableName = "BOOK_TBL"
  fromRow _c row = pure $ Book (col 0) (col 1) (col 2) (col 3) (col 4)
    where
      col i = fromSql (row !! i)

  toRow _c b = pure [toSql (book_id b), toSql (title b), toSql (author b), toSql (year b), toSql (category b)]

data BearerToken = BearerToken
  { token   :: String,
    expires :: Int
  }
  deriving (Generic, Show, Eq)

instance Entity BearerToken "token" String where
  autoIncrement = False

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

spec :: Spec
spec = do
  describe "GenericPersistence for PostgreSQL" $ do
    it "retrieves Entities using Generics" $ do
      conn <- prepareDB
      allPersons' <- select conn allEntries :: IO [Person]
      length allPersons' `shouldBe` 0
      let bob = Person 1 "Bob" 36 "7 West Street"
          alice = Person 2 "Alice" 25 "7 West Street"
      runRaw conn "INSERT INTO Person (personID, name, age, address) VALUES (1, 'Bob', 36, '7 West Street');"
      runRaw conn "INSERT INTO Person (personID, name, age, address) VALUES (2, 'Alice', 25, '7 West Street');"
      runRaw conn "INSERT INTO Person (personID, name, age, address) VALUES (3, 'Frank', 56, '7 West Street');"
      commit conn
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 3
      case allPersons of
        (p:_) -> p `shouldBe` bob
        []    -> expectationFailure "allPersons is empty"
      person' <- selectById conn (2 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Just alice
      commit conn
    it "selectById returns Nothing if no Entity was found" $ do
      conn <- prepareDB
      person' <- selectById conn (1 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Nothing
      commit conn
    it "selectById throws a DatabaseError if things go wrong in the DB" $ do
      conn <- prepareDB
      runRaw conn "DROP TABLE Person;"
      commit conn
      eitherEA <- try (selectById conn (1 :: Int) :: IO (Maybe Person))
      case eitherEA of
        Left (DatabaseError msg) -> msg `shouldContain` "does not exist"
        Left _                   -> expectationFailure "Expected DatabaseError"
        Right _                  -> expectationFailure "Expected DatabaseError"
      rollback conn
    it "retrieves Entities using user implementation" $ do
      conn <- prepareDB
      let hobbit = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937 Fiction
      runRaw conn "INSERT INTO BOOK_TBL (bookId, bookTitle, bookAuthor, bookYear, bookCategory) VALUES (1, 'The Hobbit', 'J.R.R. Tolkien', 1937, 0);"
      runRaw conn "INSERT INTO BOOK_TBL (bookId, bookTitle, bookAuthor, bookYear, bookCategory) VALUES (2, 'The Lord of the Rings', 'J.R.R. Tolkien', 1955, 0);"
      runRaw conn "INSERT INTO BOOK_TBL (bookId, bookTitle, bookAuthor, bookYear, bookCategory) VALUES (3, 'Smith of Wootton Major', 'J.R.R. Tolkien', 1967, 0);"
      commit conn
      allBooks <- select conn allEntries :: IO [Book]
      length allBooks `shouldBe` 3
      case allBooks of
        (b:_) -> b `shouldBe` hobbit
        []    -> expectationFailure "allBooks is empty"
      book' <- selectById conn (1 :: Int) :: IO (Maybe Book)
      book' `shouldBe` Just hobbit
      commit conn
    it "select returns Nothing if no Entity was found" $ do
      conn <- prepareDB
      allPersons' <- select @Person conn allEntries
      allPersons' `shouldBe` []
      commit conn
    it "select throws a DatabaseError if things go wrong" $ do
      conn <- prepareDB
      runRaw conn "DROP TABLE Person;"
      commit conn
      eitherEA <- try (select @Person conn allEntries)
      case eitherEA of
        Left (DatabaseError msg) -> msg `shouldContain` "does not exist"
        Left _                   -> expectationFailure "Expected DatabaseError"
        Right _                  -> expectationFailure "Expected DatabaseError"
      rollback conn
    it "upserts new Entities using Generics" $ do
      conn <- prepareDB
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 0
      upsert conn person
      commit conn
      allPersons' <- select conn allEntries :: IO [Person]
      length allPersons' `shouldBe` 1
      person' <- selectById conn (123456 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Just person
      commit conn
    it "upserts new Entities using user implementation" $ do
      conn <- prepareDB
      allbooks <- select conn allEntries :: IO [Book]
      length allbooks `shouldBe` 0
      upsert conn book
      commit conn
      allbooks' <- select conn allEntries :: IO [Book]
      length allbooks' `shouldBe` 1
      book' <- selectById conn (1 :: Int) :: IO (Maybe Book)
      book' `shouldBe` Just book
      commit conn
    it "upsert throws an exception if things go wrong" $ do
      conn <- prepareDB
      runRaw conn "DROP TABLE BOOK_TBL;"
      commit conn
      eitherEA <- try (upsert conn book)
      case eitherEA of
        Left (DatabaseError msg) -> msg `shouldContain` "does not exist"
        Left _                   -> expectationFailure "Expected DatabaseError"
        Right _                  -> expectationFailure "Expected DatabaseError"
    it "upserts existing Entities using Generics" $ do
      conn <- prepareDB
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 0
      upsert conn person
      commit conn
      allPersons' <- select conn allEntries :: IO [Person]
      length allPersons' `shouldBe` 1
      upsert conn person {age = 26}
      commit conn
      person' <- selectById conn (123456 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Just person {age = 26}
      commit conn
    it "upserts existing Entities using user implementation" $ do
      conn <- prepareDB
      allbooks <- select conn allEntries :: IO [Book]
      length allbooks `shouldBe` 0
      upsert conn book
      commit conn
      allbooks' <- select conn allEntries :: IO [Book]
      length allbooks' `shouldBe` 1
      eitherEA <- try $ upsert conn book {year = 1938} :: IO (Either PersistenceException ())
      case eitherEA of
        Left _  -> expectationFailure "should not throw an exception"
        Right x -> x `shouldBe` ()
      book' <- selectById conn (1 :: Int) :: IO (Maybe Book)
      book' `shouldBe` Just book {year = 1938}
      commit conn
    it "can upsert Entities with AutoIncrement" $ do
      conn <- prepareDB
      let car = Car 1 "Honda Jazz"
      -- insert the car
      upsert conn car
      car' <- selectById conn (1 :: Int) :: IO (Maybe Car)
      car' `shouldBe` Just car
      -- now update the car
      let car2 = Car 1 "Honda Civic"
      upsert conn car2
      car2' <- selectById conn (1 :: Int) :: IO (Maybe Car)
      car2' `shouldBe` Just car2
      commit conn

    it "can upsert Entities without autoIncrement" $ do
      conn <- prepareDB
      let boat = Boat 1 "Sailboat"
      -- insert the boat
      upsert conn boat
      boat' <- selectById conn (1 :: Int) :: IO (Maybe Boat)
      boat' `shouldBe` Just boat
      -- now update the boat
      let boat2 = Boat 1 "Motorboat"
      upsert conn boat2
      boat2' <- selectById conn (1 :: Int) :: IO (Maybe Boat)
      boat2' `shouldBe` Just boat2
      commit conn
    it "can handle tables with non-numeric primary keys" $ do
      conn <- prepareDB
      let token = BearerToken "secret token" 202411271659
      upsert conn token
      token' <- selectById @BearerToken conn "secret token"
      token' `shouldBe` Just token
      commit conn
    it "inserts Entities using Generics" $ do
      conn <- prepareDB
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 0
      _ <- insert conn person
      commit conn
      allPersons' <- select conn allEntries :: IO [Person]
      length allPersons' `shouldBe` 1
      person' <- selectById conn (123456 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Just person
      commit conn
    it "inserts Entities with autoincrement handling" $ do
      conn <- prepareDB
      myCar@(Car carId _) <- insert conn (Car 0 "Honda Jazz")
      myCar' <- selectById conn carId :: IO (Maybe Car)
      myCar' `shouldBe` Just myCar
      commit conn
    it "inserts many Entities re-using a single prepared stmt" $ do
      conn <- prepareDB
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 0
      insertMany conn manyPersons
      commit conn
      allPersons' <- select conn allEntries :: IO [Person]
      length allPersons' `shouldBe` 6
      commit conn
    it "updates many Entities re-using a single prepared stmt" $ do
      conn <- prepareDB
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 0
      insertMany conn manyPersons
      commit conn
      allPersons' <- select conn allEntries :: IO [Person]
      length allPersons' `shouldBe` 6
      let manyPersons' = map (\p -> p {name = "Bob"}) manyPersons
      updateMany conn manyPersons'
      commit conn
      allPersons'' <- select conn allEntries :: IO [Person]
      all (\p -> name p == "Bob") allPersons'' `shouldBe` True
      commit conn
    it "deletes many Entities re-using a single prepared stmt" $ do
      conn <- prepareDB
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 0
      insertMany conn manyPersons
      commit conn
      allPersons' <- select conn allEntries :: IO [Person]
      length allPersons' `shouldBe` 6
      deleteMany conn allPersons'
      commit conn
      allPersons'' <- select conn allEntries :: IO [Person]
      length allPersons'' `shouldBe` 0
      commit conn
    it "inserts Entities using user implementation" $ do
      conn <- prepareDB
      allbooks <- select conn allEntries :: IO [Book]
      length allbooks `shouldBe` 0
      _ <- insert conn book
      commit conn
      allbooks' <- select conn allEntries :: IO [Book]
      length allbooks' `shouldBe` 1
      book' <- selectById conn (1 :: Int) :: IO (Maybe Book)
      book' `shouldBe` Just book
      commit conn
    it "updates Entities using Generics" $ do
      conn <- prepareDB
      _ <- insert conn person
      update conn person {name = "Bob"}
      commit conn
      person' <- selectById conn (123456 :: Int) :: IO (Maybe Person)
      person' `shouldBe` Just person {name = "Bob"}
      commit conn
    it "updates Entities using user implementation" $ do
      conn <- prepareDB
      _ <- insert conn book
      update conn book {title = "The Lord of the Rings"}
      commit conn
      book' <- selectById conn (1 :: Int) :: IO (Maybe Book)
      book' `shouldBe` Just book {title = "The Lord of the Rings"}
      commit conn
    it "deletes Entities using Generics" $ do
      conn <- prepareDB
      _ <- insert conn person
      allPersons <- select conn allEntries :: IO [Person]
      length allPersons `shouldBe` 1
      delete conn person
      allPersons' <- select conn allEntries :: IO [Person]
      length allPersons' `shouldBe` 0
      commit conn
    it "deletes Entities using user implementation" $ do
      conn <- prepareDB
      _ <- insert conn book
      allBooks <- select conn allEntries :: IO [Book]
      length allBooks `shouldBe` 1
      delete conn book
      allBooks' <- select conn allEntries :: IO [Book]
      length allBooks' `shouldBe` 0
      commit conn
    it "provides a Connection Pool" $ do
      connPool <- postgreSQLPool "host=localhost dbname=postgres user=postgres password=admin port=5431"
      withResource connPool $ \conn -> do
        setupTable @Person conn defaultPostgresMapping
        _ <- insert conn person
        allPersons <- select conn allEntries :: IO [Person]
        length allPersons `shouldBe` 1
        commit conn

postgreSQLPool :: String -> IO ConnectionPool
postgreSQLPool connectString = createConnPool ExplicitCommit connectString connectPostgreSQL 10 100
