{-# LANGUAGE DeriveAnyClass     #-}  -- allows automatic derivation from Entity type class
module Reference (main) where

import           Data.Data             
import           Database.HDBC         
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           GenericPersistence    
import SqlGenerator (createTableStmtFor, dropTableStmtFor)
import Data.Maybe
import RIO


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
  deriving (Data, Entity, Show)  

instance Entity Article where
  fieldsToColumns :: Article -> [(String, String)]
  fieldsToColumns _ = [("articleID", "articleID"),
                       ("title", "title"), 
                       ("authorID", "authorID"),
                       ("year", "year")
                      ]

  fromRow row = do
    maybeAuthor <- retrieveById (row !! 2) :: GP (Maybe Author)
    let author = fromMaybe (error "Author not found") maybeAuthor
    pure $ Article (col 0) (col 1) author (col 3)
    where
      col i = fromSql (row !! i)
      

  toRow a = do 
    persist (author a)
    return [toSql (articleID a), toSql (title a), toSql $ authorID (author a), toSql (year a)]



article :: Article
article = Article 
  { articleID = 1, 
    title = "Persistence without Boilerplate", 
    author = Author 
      {authorID = 2, 
      name = "Arthur Miller", 
      address = "Denver"}, 
    year = 2018}

main :: IO ()
main = do
  -- connect to a database
  conn <- connectSqlite3 "sqlite.db"
  let ctx = Ctx (ConnWrapper conn) mempty 
  runRIO ctx $ do

    _ <- setupTableFor :: GP Article
    _ <- setupTableFor :: GP Author
  
    insert article
    liftIO $ putStrLn "OK"
  
    article' <- retrieveById "1" :: GP (Maybe Article)
    liftIO $ print article'
  
    persist article {title = "Persistence without Boilerplate (updated)"}
  
    article'' <- retrieveById "1" :: GP (Maybe Article)
    liftIO $ print article''
  
    liftIO $ print $ dropTableStmtFor (typeInfo article)
    liftIO $ print $ createTableStmtFor (typeInfo article)
  
    delete article
  
    allArticles <- retrieveAll :: GP [Article]
    liftIO $ print allArticles

  -- close connection
  disconnect conn






