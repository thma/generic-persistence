module ExceptionsSpec
  ( test,
    spec,
  )
where

import           Control.Exception
import           Database.GP.GenericPersistenceSafe
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
  conn <- connect AutoCommit <$> connectSqlite3 ":memory:"
  setupTable @Article conn defaultSqliteMapping
  return conn

data Article = Article
  { articleID :: Int,
    title     :: String,
    year      :: Int
  }
  deriving (Generic, Show, Eq)

instance Entity Article "articleID" Int where
  autoIncrement = False

data Bogus = Bogus
  { bogusID    :: Int,
    bogusTitle :: String,
    bogusYear  :: Int
  }
  deriving (Generic, Show, Eq)

instance Entity Bogus "bogusID" Int where
  tableName = "Article"

  fieldsToColumns :: [(String, String)] -- ommitting the articles field,
  fieldsToColumns =
    -- as this can not be mapped to a single column
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
      eitherExRes <- insert conn article :: IO (Either PersistenceException Article)
      case eitherExRes of
        Left di@(DuplicateInsert _msg) -> show di `shouldContain` "Entity already exists"
        _ -> expectationFailure "Expected DuplicateInsert exception"
    it "detects duplicate inserts in insertMany" $ do
      conn <- prepareDB
      _ <- insert conn article
      eitherExRes <- insertMany conn [article, article] :: IO (Either PersistenceException ())
      case eitherExRes of
        Left (DuplicateInsert msg) -> msg `shouldContain` "Entity already exists"
        _ -> expectationFailure "Expected DuplicateInsert exception"
    it "returns () for successful upsert" $ do
      conn <- prepareDB
      eitherExRes <- upsert conn article :: IO (Either PersistenceException ())
      case eitherExRes of
        Right () -> expectationSuccess
        _        -> expectationFailure "Expected ()"
    it "detects missing entities in selectById" $ do
      conn <- prepareDB
      eitherExRes <- selectById conn (1::Int) :: IO (Either PersistenceException Article)
      case eitherExRes of
        Left (EntityNotFound msg) -> msg `shouldContain` "not found"
        _ -> expectationFailure "Expected EntityNotFound exception"
    it "detects non unique entities in selectById" $ do
      conn <- connect AutoCommit <$> connectSqlite3 ":memory:"
      runRaw conn "CREATE TABLE article (articleID INTEGER, title TEXT, year INTEGER)"
      _ <- insert conn article
      _ <- insert conn article {title = "another title"}
      eitherExRes <- selectById conn (1::Int) :: IO (Either PersistenceException Article)
      case eitherExRes of
        Left (NoUniqueKey msg) -> msg `shouldContain` "More than one Article found for id SqlInt64 1"
        _ -> expectationFailure "Expected DuplicateEntity exception"

    it "detects bogus data in selectById" $ do
      conn <- prepareDB
      _ <- insert conn article
      eitherExRes <- selectById conn (1::Int) :: IO (Either PersistenceException Bogus)
      case eitherExRes of
        Left (DatabaseError msg) -> msg `shouldContain` "can't create bogus"
        Left pe -> expectationFailure $ "Expected DatabaseError exception, got " ++ show pe
        _ -> expectationFailure "Expected DatabaseError exception"

    it "detects missing entities in update" $ do
      conn <- prepareDB
      eitherExRes <- update conn article :: IO (Either PersistenceException ())
      case eitherExRes of
        Left (EntityNotFound msg) -> msg `shouldContain` "does not exist"
        _ -> expectationFailure "Expected EntityNotFound exception"
    it "detects missing entities in delete" $ do
      conn <- prepareDB
      eitherExRes <- delete conn article :: IO (Either PersistenceException ())
      case eitherExRes of
        Left (EntityNotFound msg) -> msg `shouldContain` "does not exist"
        _ -> expectationFailure "Right: Expected EntityNotFound exception"
    it "detects missing entities in deleteById" $ do
      conn <- prepareDB
      eitherExRes <- deleteById @Article conn (1::Int) :: IO (Either PersistenceException ())
      case eitherExRes of
        Left (EntityNotFound msg) -> msg `shouldContain` "does not exist"
        _ -> expectationFailure "Expected EntityNotFound exception"
    it "detects general db issues in deleteById" $ do
      conn <- connect AutoCommit <$> connectSqlite3 ":memory:"
      eitherExRes <- deleteById @Article conn (1::Int) :: IO (Either PersistenceException ())
      case eitherExRes of
        Left (DatabaseError msg) -> msg `shouldContain` "SqlError"
        _ -> expectationFailure "Expected DatabaseError exception"
    it "detects general backend issues" $ do
      conn <- connect AutoCommit <$> connectSqlite3 ":memory:"
      eitherExRes <- update conn article :: IO (Either PersistenceException ())
      case eitherExRes of
        Left (DatabaseError msg) -> msg `shouldContain` "SqlError"
        _ -> expectationFailure "Expected DatabaseError exception"
    it "has no leaking backend exceptions" $ do
      conn <- connect AutoCommit <$> connectSqlite3 ":memory:"
      _ <- update conn article :: IO (Either PersistenceException ())
      _ <- insert conn article :: IO (Either PersistenceException Article)
      _ <- upsert conn article :: IO (Either PersistenceException ())
      _ <- delete conn article :: IO (Either PersistenceException ())
      _ <- selectById conn (1::Int) :: IO (Either PersistenceException Article)
      _ <- select conn allEntries :: IO (Either PersistenceException [Article])
      _ <- select conn (yearField =. "2023") :: IO (Either PersistenceException [Article])
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
      handleDuplicateInsert (toException $ EntityNotFound "24") `shouldBe` DatabaseError "EntityNotFound \"24\""

    it "handles autoincrement edge cases" $ do
      conn <- connect AutoCommit <$> connectSqlite3 ":memory:"
      let article' = Article 200 "title" 2023
          expectedRow = [SqlInt64 200, SqlString "title", SqlInt64 2023]
      articleRow <- toRow conn article'
      articleRow `shouldBe` expectedRow
      -- Article is defined with autoIncrement = False
      removeAutoIncIdField @Article articleRow `shouldBe` expectedRow

      -- ArticleWithoutPK is defined with autoIncrement = True, but DB can't auto-increment the primary key type
      let articleWithoutPK = ArticleWithoutPK "title" 2023
          rowWithoutPK = [SqlInt64 2023]
      articleRowWithoutPK <- toRow conn articleWithoutPK
      removeAutoIncIdField @ArticleWithoutPK articleRowWithoutPK `shouldBe` rowWithoutPK

      -- ArticleWithPKatEnd is defined with autoIncrement = True and has a primary key field as last column
      let articleWithPKatEnd = ArticleWithPKatEnd "title" 2023 200
          expectedRow2 = [SqlString "title", SqlInt64 2023]
      articleRowWithPKatEnd <- toRow conn articleWithPKatEnd
      removeAutoIncIdField @ArticleWithPKatEnd articleRowWithPKatEnd `shouldBe` expectedRow2

data ArticleWithoutPK = ArticleWithoutPK
  { title1 :: String,
    year1  :: Int
  }
  deriving (Generic, Show, Eq)

instance Entity ArticleWithoutPK "title1" String where
  autoIncrement = True

data ArticleWithPKatEnd = ArticleWithPKatEnd
  { title2     :: String,
    year2      :: Int,
    articleID2 :: Int
  }
  deriving (Generic, Show, Eq)

instance Entity ArticleWithPKatEnd "articleID2" Int where
  autoIncrement = True
