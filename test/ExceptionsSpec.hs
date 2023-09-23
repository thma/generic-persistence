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
        Left (DuplicateInsert msg) -> do print msg; expectationSuccess
        _                        -> expectationFailure "Expected DuplicateInsert exception"
    it "detects duplicate inserts in insertMany" $ do
      conn <- prepareDB
      _ <- insert conn article
      eitherExRes <- insertMany conn [article,article] :: IO (Either PersistenceException ())
      case eitherExRes of
        Left (DuplicateInsert msg) -> do print msg; expectationSuccess
        _                        -> expectationFailure "Expected DuplicateInsert exception"
    it "detects missing entities in selectById" $ do
      conn <- prepareDB
      eitherExRes <- selectById conn "1" :: IO (Either PersistenceException Article)
      case eitherExRes of
        Left (EntityNotFound msg) -> do print msg; expectationSuccess
        _                       -> expectationFailure "Expected EntityNotFound exception"
    it "detects non unique entities in selectById" $ do
      conn <- connect SQLite <$> connectSqlite3 ":memory:"
      runRaw conn "CREATE TABLE article (articleID INTEGER, title TEXT, year INTEGER)"
      _ <- insert conn article
      _ <- insert conn article{title="another title"}
      eitherExRes <- selectById conn "1" :: IO (Either PersistenceException Article)
      case eitherExRes of
        Left (NoUniqueKey msg) -> do print msg; expectationSuccess
        _                          -> expectationFailure "Expected DuplicateEntity exception"
    it "detects missing entities in update" $ do
      conn <- prepareDB
      eitherExRes <- update conn article :: IO (Either PersistenceException ())
      case eitherExRes of
        Left (EntityNotFound msg) -> do print msg; expectationSuccess
        _                       -> expectationFailure "Expected EntityNotFound exception"
    it "detects missing entities in delete" $ do
      conn <- prepareDB
      eitherExRes <- delete conn article :: IO (Either PersistenceException ())
      case eitherExRes of
        Left (EntityNotFound msg) -> do print msg; expectationSuccess
        _                       -> expectationFailure "Right: Expected EntityNotFound exception"
    it "detects general backend issues" $ do
      conn <- connect SQLite <$> connectSqlite3 ":memory:"
      eitherExRes <- update conn article :: IO (Either PersistenceException ())
      case eitherExRes of
        Left (DatabaseError msg) -> do print msg; expectationSuccess
        _                      -> expectationFailure "Expected DatabaseError exception"
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
    
