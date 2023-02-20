{-# LANGUAGE DeriveAnyClass     #-}  -- allows automatic derivation from Entity type class
module ReferenceSpec
  ( test
  , spec
  ) where

import          Test.Hspec
import          Data.Data
import          Database.HDBC
import          Database.HDBC.Sqlite3
import          Database.GP.GenericPersistence
import          Data.Maybe
import          GHC.Generics

-- `test` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery. (start up stack repl --test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

prepareDB :: IO Conn
prepareDB = do
  conn <- ConnWrapper <$> connectSqlite3 ":memory:"
  _ <- setupTableFor conn :: IO Article
  _ <- setupTableFor conn :: IO Author
  return conn

data Article = Article
  { articleID :: Int,
    title     :: String,
    author    :: Author,
    year      :: Int
  }
  deriving (Generic, Data, Show, Eq)

data Author = Author
  { authorID :: Int,
    name     :: String,
    address  :: String
  }
  deriving (Generic, Data, Entity, Show, Eq)  

instance Entity Article where
  fieldsToColumns :: Article -> [(String, String)]
  fieldsToColumns _ = [("articleID", "articleID"),
                       ("title", "title"), 
                       ("authorID", "authorID"),
                       ("year", "year")
                      ]

  fromRow conn row = do
    maybeAuthor <- retrieveById conn (row !! 2) :: IO (Maybe Author)
    let author = fromMaybe (error "Author not found") maybeAuthor
    pure $ rawArticle {author = author}
    where
      rawArticle = fromRowWoCtx row
      
  toRow conn a = do 
    persist conn (author a)
    return $ toRowWoCtx a 

toRowWoCtx :: Article -> [SqlValue]
toRowWoCtx a = [toSql (articleID a), toSql (title a), toSql $ authorID (author a), toSql (year a)]

fromRowWoCtx :: [SqlValue] -> Article
fromRowWoCtx row = Article (col 0) (col 1) (Author (col 2) "" "") (col 3)
  where
    col i = fromSql (row !! i)

article :: Article
article = Article 
  { articleID = 1, 
    title = "Persistence without Boilerplate", 
    author = arthur, 
    year = 2018}
    
arthur :: Author
arthur = Author 
  {authorID = 2, 
  name = "Arthur Miller", 
  address = "Denver"}    

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
        



