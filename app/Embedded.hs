{-# LANGUAGE DeriveAnyClass     #-}  -- allows automatic derivation from Entity type class

module Embedded (main) where

--import           Data.Data             (Data)
import           Database.HDBC         
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           GenericPersistence    
import           RIO


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

  fromRow row = return $ Article (col 0) (col 1) author (col 5)
    where
      col i = fromSql (row !! i)
      author = Author (col 2) (col 3) (col 4)

  toRow  a = return [toSql (articleID a), toSql (title a), toSql authID, toSql authorName, toSql authorAddress, toSql (year a)]
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
  let ctx = Ctx (ConnWrapper conn) mempty
  runRIO ctx $ do

    _ <- setupTableFor :: GP Article
  
    insert article
  
    article' <- retrieveById "1" :: GP (Maybe Article)
    liftIO $ print article'
  
    persist article {title = "Persistence without Boilerplate (updated)"}
  
    article'' <- retrieveById "1" :: GP (Maybe Article)
    liftIO $ print article''
  
    delete article
  
    allArticles <- retrieveAll :: GP [Article]
    liftIO $ print allArticles

  -- close connection
  disconnect conn






