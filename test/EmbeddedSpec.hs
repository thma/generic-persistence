{-# LANGUAGE DeriveAnyClass #-}

module EmbeddedSpec
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
  conn <- connect AutoCommit <$> connectSqlite3 ":memory:"
  setupTableFor @Article SQLite conn
  return conn

data Article = Article
  { articleID :: Int,
    title     :: String,
    author    :: Author,
    year      :: Int
  }
  deriving (Generic, Show, Eq)

data Author = Author
  { authorID :: Int,
    name     :: String,
    address  :: String
  }
  deriving (Generic, Show, Eq)

instance Entity Article where
  fieldsToColumns =
    [ ("articleID", "articleID"),
      ("title", "title"),
      ("authorID", "authorID"),
      ("authorName", "authorName"),
      ("authorAddress", "authorAddress"),
      ("year", "year")
    ]

  fromRow :: Conn -> [SqlValue] -> IO Article
  fromRow _ r = return $ fromRowWoCtx r
    where
      fromRowWoCtx row = Article (col 0) (col 1) author (col 5)
        where
          col i = fromSql (row !! i)
          author = Author (col 2) (col 3) (col 4)

  toRow _ art = return $ toRowWoCtx art
    where
      toRowWoCtx a = [toSql (articleID a), toSql (title a), toSql authID, toSql authorName, toSql authorAddress, toSql (year a)]
      authID = authorID (author art)
      authorName = name (author art)
      authorAddress = address (author art)

article :: Article
article =
  Article
    { articleID = 1,
      title = "Persistence without Boilerplate",
      author =
        Author
          { authorID = 1,
            name = "Arthur Dent",
            address = "Boston"
          },
      year = 2018
    }

spec :: Spec
spec = do
  describe "Handling of Embedded Objects" $ do
    it "works like a charm" $ do
      conn <- prepareDB
      _ <- insert conn article
      article' <- selectById conn "1" :: IO (Maybe Article)
      article' `shouldBe` Just article
      allArticles <- select conn allEntries :: IO [Article]
      allArticles `shouldBe` [article]
