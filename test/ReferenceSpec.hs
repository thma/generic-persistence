-- allows automatic derivation from Entity type class
{-# LANGUAGE DeriveAnyClass #-}

module ReferenceSpec
  ( test,
    spec,
  )
where

import           Data.Maybe
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
  conn <- connect SQLite <$> connectSqlite3 ":memory:"
  setupTableFor @Article conn
  setupTableFor @Author conn
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
  deriving (Generic, Entity, Show, Eq)

instance Entity Article where
  fieldsToColumns :: [(String, String)]                      -- ommitting the author field,
  fieldsToColumns =                                          -- as this can not be mapped to a single column
    [ ("articleID", "articleID"),                            -- instead we invent a new column authorID         
      ("title", "title"),
      ("authorID", "authorID"),
      ("year", "year")
    ]

  fromRow :: Conn -> [SqlValue] -> IO Article
  fromRow conn row = do    
    authorById <- fromJust <$> retrieveById conn (row !! 2)  -- load author by foreign key
    return $ rawArticle {author = authorById}                -- add author to article
    where
      rawArticle = Article (col 0) (col 1)                   -- create article from row, 
                           (Author (col 2) "" "") (col 3)    -- using a dummy author
        where
          col i = fromSql (row !! i)

  toRow :: Conn -> Article -> IO [SqlValue]
  toRow conn a = do
    persist conn (author a)                                  -- persist author first
    return [toSql (articleID a), toSql (title a),            -- return row for article table where 
            toSql $ authorID (author a), toSql (year a)]     -- authorID is foreign key to author table 



article :: Article
article =
  Article
    { articleID = 1,
      title = "Persistence without Boilerplate",
      author = arthur,
      year = 2018
    }

arthur :: Author
arthur =
  Author
    { authorID = 2,
      name = "Arthur Miller",
      address = "Denver"
    }

spec :: Spec
spec = do
  describe "Handling of 1:1 References" $ do
    it "works like a charm" $ do
      conn <- prepareDB
      insert conn article

      author' <- retrieveById conn "2" :: IO (Maybe Author)
      author' `shouldBe` Just arthur

      article' <- retrieveById conn "1" :: IO (Maybe Article)
      article' `shouldBe` Just article
