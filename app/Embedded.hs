{-# LANGUAGE DeriveAnyClass     #-}  -- allows automatic derivation from Entity type class

module Embedded (main) where

import           Data.Data             (Data)
import           Database.HDBC         
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           GenericPersistence    


data Article = Article
  { articleID :: Int,
    title     :: String,
    author    :: Author,
    year      :: Int
  }
  deriving (Data, Show)

data Author = Author
  { authorID :: Int,
    name     :: String,
    address  :: String
  }
  deriving (Data, Show)  

instance Entity Article where

  fieldsToColumns :: Article -> [(String, String)]
  fieldsToColumns _ = [("articleID", "articleID"),
                       ("title", "title"), 
                       ("authorID", "authorID"), 
                       ("authorName", "authorName"), 
                       ("authorAddress", "authorAddress"),
                       ("year", "year")
                      ]

  fromRow :: conn -> ResolutionCache -> [SqlValue] -> IO Article
  fromRow _conn _rc row = return $ Article (col 0) (col 1) author (col 5)
    where
      col i = fromSql (row !! i)
      author = Author (col 2) (col 3) (col 4)

  toRow :: conn -> ResolutionCache -> Article -> IO [SqlValue]
  toRow _conn _rc a = return [toSql (articleID a), toSql (title a), toSql authID, toSql authorName, toSql authorAddress, toSql (year a)]
    where 
      authID = authorID (author a)
      authorName = name (author a)
      authorAddress = address (author a)

article :: Article
article = Article 
  { articleID = 1, 
    title = "Persistence without Boilerplate", 
    author = Author 
      {authorID = 1, 
      name = "Arthur Dent", 
      address = "Boston"}, 
    year = 2018}

main :: IO ()
main = do
  -- connect to a database
  conn <- connectSqlite3 "sqlite.db"

  _ <- setupTableFor conn :: IO Article

  insert conn article

  article' <- retrieveById conn mempty "1" :: IO (Maybe Article)
  print article'

  persist conn article {title = "Persistence without Boilerplate (updated)"}

  article'' <- retrieveById conn mempty "1" :: IO (Maybe Article)
  print article''

  delete conn article

  allArticles <- retrieveAll conn mempty :: IO [Article]
  print allArticles

  -- close connection
  disconnect conn






