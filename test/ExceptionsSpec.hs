-- allows automatic derivation from Entity type class
{-# LANGUAGE DeriveAnyClass #-}

module ExceptionsSpec
  ( test,
    spec,
  )
where

import           Database.GP.GenericPersistenceSafe
import           Database.HDBC.Sqlite3
import           GHC.Generics
import           Test.Hspec
import           Database.HDBC
import           Control.Exception

-- `test` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery. 
-- (start up stack repl --test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

prepareDB :: IO Conn
prepareDB = do
  conn <- connect SQLite <$> connectSqlite3 ":memory:"
  setupTableFor @Article conn
  return conn

data Article = Article
  { articleID :: Int,
    title     :: String,
    year      :: Int
  }
  deriving (Generic, Entity, Show, Eq)

data Bogus = Bogus
  { bogusID :: Int,
    bogusTitle :: String,
    bogusYear :: Int
  }
  deriving (Generic, Show, Eq)

instance Entity Bogus where
  tableName = "Article"

  fieldsToColumns :: [(String, String)]                  -- ommitting the articles field, 
  fieldsToColumns =                                      -- as this can not be mapped to a single column
    [ ("bogusID", "articleID"),
      ("bogusTitle", "title"),
      ("bogusYear", "year")
    ]

  fromRow :: Conn -> [SqlValue] -> IO Bogus
  fromRow _conn _row = throw $ DatabaseError "can't create bogus"


yearField :: Field
yearField = field "year"

article :: Article
article = Article 1 "The Hitchhiker's Guide to the Galaxy" 1979

expectationSuccess :: IO ()
expectationSuccess = return ()

spec :: Spec
spec = do
  describe "Exception Handling" $ do
    it "detects duplicate inserts" $ do
      conn <- prepareDB
      _ <- insert conn article
      eitherExRes <- insert conn article :: IO (Either PersistenceException ())
      case eitherExRes of
        Left di@(DuplicateInsert _msg) -> show di `shouldContain` "Entity already exists"
        _                          -> expectationFailure "Expected DuplicateInsert exception"
    it "detects duplicate inserts in insertMany" $ do
      conn <- prepareDB
      _ <- insert conn article
      eitherExRes <- insertMany conn [article,article] :: IO (Either PersistenceException ())
      case eitherExRes of
        Left (DuplicateInsert msg) -> msg `shouldContain` "Entity already exists"
        _                          -> expectationFailure "Expected DuplicateInsert exception"
    it "detects duplicate inserts in persist" $ do
      conn <- connect SQLite <$> connectSqlite3 ":memory:"
      runRaw conn "CREATE TABLE article (articleID INTEGER, title TEXT, year INTEGER)"
      _ <- insert conn article
      _ <- insert conn article{title="another title"}
      eitherExRes <- persist conn article :: IO (Either PersistenceException ())
      case eitherExRes of
        Left (NoUniqueKey msg) -> msg `shouldContain` "More than one entity found for id SqlInt64 1"
        Left pe                -> expectationFailure $ "Expected NoUniqueKey exception, got " ++ show pe
        _                      -> expectationFailure "Expected NoUniqueKey exception"
    it "detects missing entities in selectById" $ do
      conn <- prepareDB
      eitherExRes <- selectById conn "1" :: IO (Either PersistenceException Article)
      case eitherExRes of
        Left (EntityNotFound msg) -> msg `shouldContain` "not found"
        _                         -> expectationFailure "Expected EntityNotFound exception"
    it "detects non unique entities in selectById" $ do
      conn <- connect SQLite <$> connectSqlite3 ":memory:"
      runRaw conn "CREATE TABLE article (articleID INTEGER, title TEXT, year INTEGER)"
      _ <- insert conn article
      _ <- insert conn article{title="another title"}
      eitherExRes <- selectById conn "1" :: IO (Either PersistenceException Article)
      case eitherExRes of
        Left (NoUniqueKey msg) -> msg `shouldContain` "More than one Article found for id SqlString \"1\""
        _                      -> expectationFailure "Expected DuplicateEntity exception"

    it "detects bogus data in selectById" $ do
      conn <- prepareDB
      _ <- insert conn article
      eitherExRes <- selectById conn "1" :: IO (Either PersistenceException Bogus)
      case eitherExRes of
        Left (DatabaseError msg) -> msg `shouldContain` "can't create bogus"
        Left pe                  -> expectationFailure $ "Expected DatabaseError exception, got " ++ show pe
        _                        -> expectationFailure "Expected DatabaseError exception"

    it "detects missing entities in update" $ do
      conn <- prepareDB
      eitherExRes <- update conn article :: IO (Either PersistenceException ())
      case eitherExRes of
        Left (EntityNotFound msg) -> msg `shouldContain` "does not exist"
        _                         -> expectationFailure "Expected EntityNotFound exception"
    it "detects missing entities in delete" $ do
      conn <- prepareDB
      eitherExRes <- delete conn article :: IO (Either PersistenceException ())
      case eitherExRes of
        Left (EntityNotFound msg) -> msg `shouldContain` "does not exist"
        _                         -> expectationFailure "Right: Expected EntityNotFound exception"
    it "detects general backend issues" $ do
      conn <- connect SQLite <$> connectSqlite3 ":memory:"
      eitherExRes <- update conn article :: IO (Either PersistenceException ())
      case eitherExRes of
        Left (DatabaseError msg) -> msg `shouldContain` "SqlError"
        _                        -> expectationFailure "Expected DatabaseError exception"
    it "has no leaking backend exceptions" $ do
      conn <- connect SQLite <$> connectSqlite3 ":memory:"
      _ <- update conn article :: IO (Either PersistenceException ())
      _ <- insert conn article :: IO (Either PersistenceException ())
      _ <- persist conn article :: IO (Either PersistenceException ())
      _ <- delete conn article :: IO (Either PersistenceException ())
      _ <- selectById conn "1" :: IO (Either PersistenceException Article)
      _ <- select conn allEntries :: IO (Either PersistenceException [Article])
      _ <- select conn (yearField =. "2023")  :: IO (Either PersistenceException [Article])
      _ <- insertMany conn [article] :: IO (Either PersistenceException ())
      _ <- updateMany conn [article] :: IO (Either PersistenceException ())
      _ <- deleteMany conn [article] :: IO (Either PersistenceException ())
      expectationSuccess

    it "can find column names" $ do
      let columnName = columnNameFor @Article "articleID"
      columnName `shouldBe` "articleID"

    it "throws an exception when column name is not found" $ do
      let columnName = columnNameFor @Article "unknown"
      print columnName `shouldThrow` errorCall "columnNameFor: Article has no column mapping for unknown"

    it "reports unknown fields" $ do
      let index = fieldIndex @Article "unknown"
      print index `shouldThrow` errorCall "expectJust Field unknown is not present in type Article"

    it "handles duplicate insert exceptions" $ do
      handleDuplicateInsert (toException $ EntityNotFound "24" ) `shouldBe` DatabaseError "EntityNotFound \"24\""
    
