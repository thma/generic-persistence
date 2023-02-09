{-# LANGUAGE DeriveAnyClass     #-}  -- allows automatic derivation from Entity type class
module GenericPersistenceSpec
  ( test
  , spec
  , withDatabase
  ) where


import           Test.Hspec
import           Data.Data             
import           Database.HDBC         
import           Database.HDBC.Sqlite3
import           Database.GP.GenericPersistence
import           RIO



-- `test` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery. (start up stack repl --test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

withDatabase :: RIO Ctx a -> IO a
withDatabase action = do
  conn <- connectSqlite3 ":memory:"
  let ctx = Ctx (ConnWrapper conn) mempty
  runRIO ctx $ do
    _ <- setupTableFor :: GP Person
    _ <- setupTableFor :: GP Book
    action

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
  fromRow row = pure $ Book (col 0) (col 1) (col 2) (col 3) (col 4)
    where
      col i = fromSql (row !! i)

  toRow b = pure [toSql (book_id b), toSql (title b), toSql (author b), toSql (year b), toSql (category b)]


person :: Person
person = Person 123456 "Alice" 25 "123 Main St"

book :: Book
book = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937 Fiction


spec :: Spec
spec = do
  describe "GenericPersistence" $ do
    it "retrieves Entities using Generics" $ 
      withDatabase $ do
        let bob = Person 1 "Bob" 36 "7 West Street"
        Ctx conn _ <- ask
        liftIO $ runRaw conn "INSERT INTO Person (personID, name, age, address) VALUES (1, \"Bob\", 36, \"7 West Street\");"
        liftIO $ runRaw conn "INSERT INTO Person (personID, name, age, address) VALUES (2, \"Alice\", 25, \"7 West Street\");"
        liftIO $ runRaw conn "INSERT INTO Person (personID, name, age, address) VALUES (3, \"Frank\", 56, \"7 West Street\");"
        allPersons <- retrieveAll :: GP [Person]
        liftIO $ length allPersons `shouldBe` 3
        liftIO $ head allPersons `shouldBe` bob
        person' <- retrieveById (1 :: Int) :: GP (Maybe Person)
        liftIO $ person' `shouldBe` Just bob
    it "retrieves Entities using user implementation" $
      withDatabase $ do
      let hobbit = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937 Fiction
      Ctx conn _ <- ask
      liftIO $ runRaw conn "INSERT INTO BOOK_TBL (bookId, bookTitle, bookAuthor, bookYear, bookCategory) VALUES (1, \"The Hobbit\", \"J.R.R. Tolkien\", 1937, 0);"
      liftIO $ runRaw conn "INSERT INTO BOOK_TBL (bookId, bookTitle, bookAuthor, bookYear, bookCategory) VALUES (2, \"The Lord of the Rings\", \"J.R.R. Tolkien\", 1955, 0);"
      liftIO $ runRaw conn "INSERT INTO BOOK_TBL (bookId, bookTitle, bookAuthor, bookYear, bookCategory) VALUES (3, \"Smith of Wootton Major\", \"J.R.R. Tolkien\", 1967, 0);"
      allBooks <- retrieveAll :: GP [Book]
      liftIO $ length allBooks `shouldBe` 3
      liftIO $ head allBooks `shouldBe` hobbit
      book' <- retrieveById(1 :: Int) :: GP (Maybe Book)
      liftIO $ book' `shouldBe` Just hobbit
    
    it "persists new Entities using Generics" $ 
      withDatabase $ do
      allPersons <- retrieveAll :: GP [Person]
      liftIO $ length allPersons `shouldBe` 0
      persist person
      allPersons' <- retrieveAll :: GP [Person]
      liftIO $ length allPersons' `shouldBe` 1
      person' <- retrieveById (123456 :: Int) :: GP (Maybe Person)
      liftIO $ person' `shouldBe` Just person
    it "persists new Entities using user implementation" $ 
      withDatabase $ do
      allbooks <- retrieveAll :: GP [Book]
      liftIO $ length allbooks `shouldBe` 0
      persist book
      allbooks' <- retrieveAll :: GP [Book]
      liftIO $ length allbooks' `shouldBe` 1
      book' <- retrieveById (1 :: Int) :: GP (Maybe Book) 
      liftIO $ book' `shouldBe` Just book
    it "persists existing Entities using Generics" $
      withDatabase $ do
      allPersons <- retrieveAll :: GP [Person]
      liftIO $ length allPersons `shouldBe` 0
      persist person
      allPersons' <- retrieveAll :: GP [Person]
      liftIO $ length allPersons' `shouldBe` 1
      persist person {age = 26}
      person' <- retrieveById (123456 :: Int) :: GP (Maybe Person)
      liftIO $ person' `shouldBe` Just person {age = 26}
    it "persists existing Entities using user implementation" $
      withDatabase $ do
      allbooks <- retrieveAll :: GP [Book]
      liftIO $ length allbooks `shouldBe` 0
      persist book
      allbooks' <- retrieveAll :: GP [Book]
      liftIO $ length allbooks' `shouldBe` 1
      persist book {year = 1938}
      book' <- retrieveById (1 :: Int) :: GP (Maybe Book)
      liftIO $ book' `shouldBe` Just book {year = 1938}
    it "inserts Entities using Generics" $
      withDatabase $ do
      allPersons <- retrieveAll :: GP [Person]
      liftIO $ length allPersons `shouldBe` 0
      insert person
      allPersons' <- retrieveAll :: GP [Person]
      liftIO $ length allPersons' `shouldBe` 1
      person' <- retrieveById (123456 :: Int) :: GP (Maybe Person)
      liftIO $ person' `shouldBe` Just person
    it "inserts Entities using user implementation" $
      withDatabase $ do
      allbooks <- retrieveAll :: GP [Book]
      liftIO $ length allbooks `shouldBe` 0
      insert book
      allbooks' <- retrieveAll :: GP [Book]
      liftIO $ length allbooks' `shouldBe` 1
      book' <- retrieveById (1 :: Int) :: GP (Maybe Book)
      liftIO $ book' `shouldBe` Just book
    it "updates Entities using Generics" $
      withDatabase $ do
      insert person
      update person {name = "Bob"}
      person' <- retrieveById (123456 :: Int) :: GP (Maybe Person)
      liftIO $ person' `shouldBe` Just person {name = "Bob"}
    it "updates Entities using user implementation" $
      withDatabase $ do
      insert book
      update book {title = "The Lord of the Rings"}
      book' <- retrieveById (1 :: Int) :: GP (Maybe Book)
      liftIO $ book' `shouldBe` Just book {title = "The Lord of the Rings"}
    it "deletes Entities using Generics" $
      withDatabase $ do
      insert person
      allPersons <- retrieveAll :: GP [Person]
      liftIO $ length allPersons `shouldBe` 1
      delete person
      allPersons' <- retrieveAll :: GP [Person]
      liftIO $ length allPersons' `shouldBe` 0  
    it "deletes Entities using user implementation" $
      withDatabase $ do
      insert book
      allBooks <- retrieveAll :: GP [Book]
      liftIO $ length allBooks `shouldBe` 1
      delete book
      allBooks' <- retrieveAll :: GP [Book]
      liftIO $ length allBooks' `shouldBe` 0
      
