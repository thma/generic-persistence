-- allows automatic derivation from Entity type class
{-# LANGUAGE DeriveAnyClass #-}

module OneToManySafeSpec
  ( test,
    spec,
  )
where

import           Database.GP.GenericPersistenceSafe
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           GHC.Generics
import           Test.Hspec
import           Data.Either                    (fromRight)

-- `test` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery. 
-- (start up stack repl -- test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

prepareDB :: IO Conn
prepareDB = do
  conn <- connect AutoCommit <$> connectSqlite3 ":memory:"
  setupTableFor @Article SQLite conn
  setupTableFor @Author SQLite conn
  return conn

data Article = Article
  { articleID :: Int,
    title     :: String,
    authorId  :: Int,
    year      :: Int
  }
  deriving (Generic, Entity, Show, Eq) -- automatically derives Entity

data Author = Author
  { authorID :: Int,
    name     :: String,
    address  :: String,
    articles :: [Article]
  }
  deriving (Generic, Show, Eq)

instance Entity Author where
  fieldsToColumns :: [(String, String)]                  -- ommitting the articles field, 
  fieldsToColumns =                                      -- as this can not be mapped to a single column
    [ ("authorID", "authorID"),
      ("name", "name"),
      ("address", "address")
    ]

  fromRow :: Conn -> [SqlValue] -> IO Author
  fromRow conn row = do
    let authID = head row                                 -- authorID is the first column
    articlesBy <- (fromRight []) <$> select @Article conn (field "authorId" =. authID) -- retrieve all articles by this author
    return rawAuthor {articles = articlesBy}              -- add the articles to the author
    where
      rawAuthor = Author (col 0) (col 1) (col 2) []       -- create the author from row (w/o articles)
      col i = fromSql (row !! i)                          -- helper function to convert SqlValue to Haskell type

  toRow :: Conn -> Author -> IO [SqlValue]
  toRow conn a = do
    mapM_ (persist conn) (articles a)                     -- persist all articles of this author (update or insert)
    return [toSql (authorID a),                           -- return the author as a list of SqlValues
            toSql (name a), toSql (address a)]


article1 :: Article
article1 =
  Article
    { articleID = 1,
      title = "Persistence without Boilerplate",
      authorId = 1,
      year = 2018
    }

article2 :: Article
article2 =
  Article
    { articleID = 2,
      title = "Boilerplate for Dummies",
      authorId = 2,
      year = 2020
    }

article3 :: Article
article3 =
  Article
    { articleID = 3,
      title = "The return of the boilerplate",
      authorId = 2,
      year = 2022
    }

arthur :: Author
arthur =
  Author
    { authorID = 2,
      name = "Arthur Miller",
      address = "Denver",
      articles = [article2, article3]
    }

spec :: Spec
spec = do
  describe "Handling of 1:N References" $ do
    it "works like a charm" $ do
      conn <- prepareDB

      eitherPeUnit <- insert conn arthur
      print eitherPeUnit
      _ <- insert conn article1

      authors <- fromRight [] <$> select @Author conn allEntries
      length authors `shouldBe` 1

      articles' <- fromRight [] <$> select @Article conn allEntries
      length articles' `shouldBe` 3

      eitherPeAuthor <- selectById @Author conn "2"
      eitherPeAuthor `shouldBe` Right arthur
      case eitherPeAuthor of
        Left _ -> fail "should not happen"
        Right author -> do
          length (articles author) `shouldBe` 2
      
      _ <- persist conn arthur {address = "New York"}
      eitherPeAuthor' <- selectById @Author conn "2"
      eitherPeAuthor' `shouldBe` Right arthur {address = "New York"}
    it "delete returns unit in case of success" $ do
      conn <- prepareDB
      _ <- insert conn arthur
      eitherPeUnit <- delete conn arthur
      eitherPeUnit `shouldBe` Right ()
    it "delete handles exceptions" $ do
      conn <- connect AutoCommit <$> connectSqlite3 ":memory:"
      eitherPeUnit <- delete conn arthur
      case eitherPeUnit of
        Left (DatabaseError msg) -> msg `shouldContain` "no such table: Author"
        _ -> fail "should not happen"
    it "insertMany works with references" $ do
      conn <- prepareDB
      let authors = [arthur, arthur{name="Bob", authorID=3, articles=[]}]
      eitherPeUnit <- insertMany conn authors
      eitherPeUnit `shouldBe` Right ()
      eitherPeAuthors <- select @Author conn allEntries
      eitherPeAuthors `shouldBe` Right authors
    it "update works with references" $ do
      conn <- prepareDB
      _ <- insert conn arthur
      eitherPeUnit <- update conn arthur {address = "New York"}
      eitherPeUnit `shouldBe` Right ()
      eitherPeAuthor <- selectById @Author conn "2"
      eitherPeAuthor `shouldBe` Right arthur {address = "New York"}
    it "updateMany works with references" $ do
      conn <- prepareDB
      let authors = [arthur, arthur{name="Bob", authorID=3, articles=[]}]
      _ <- insertMany conn authors
      eitherPeUnit <- updateMany conn (map (\a -> a {address = "New York"}) authors)
      eitherPeUnit `shouldBe` Right ()
      eitherPeAuthors <- select @Author conn allEntries
      eitherPeAuthors `shouldBe` Right (map (\a -> a {address = "New York"}) authors)
    it "deleteMany works with references" $ do
      conn <- prepareDB
      let authors = [arthur, arthur{name="Bob", authorID=3, articles=[]}]
      _ <- insertMany conn authors
      eitherPeUnit <- deleteMany conn authors
      eitherPeUnit `shouldBe` Right ()
      eitherPeAuthors <- select @Author conn allEntries
      eitherPeAuthors `shouldBe` Right []


