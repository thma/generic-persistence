{-# LANGUAGE DeriveAnyClass     #-}  -- allows automatic derivation from Entity type class
module Reference (main) where

import           Data.Data             (Data)
import           Database.HDBC         
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           GenericPersistence    
import SqlGenerator (createTableStmtFor, dropTableStmtFor)


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

 
  --fromRow :: IConnection conn => conn -> [SqlValue] -> IO Article
  fromRow conn rc row = do
    author <- retrieveById conn rc (row !! 2) :: IO Author
    pure $ Article (col 0) (col 1) author (col 3)
    where
      col i = fromSql (row !! i)
      

  --toRow :: IConnection conn => conn -> Article -> IO [SqlValue]
  toRow conn _rc a = do 
    persist conn (author a)
    return [toSql (articleID a), toSql (title a), toSql $ authorID (author a), toSql (year a)]



article :: Article
article = Article 
  { articleID = 1, 
    title = "Persistence without Boilerplate", 
    author = Author 
      {authorID = 2, 
      name = "Arthur Miller", 
      address = "Mars Colonies"}, 
    year = 2018}

main :: IO ()
main = do
  -- connect to a database
  conn <- connectSqlite3 "sqlite.db"

  runRaw conn "DROP TABLE IF EXISTS Article"
  runRaw conn "CREATE TABLE Article (articleID INTEGER PRIMARY KEY, title TEXT, authorID INTEGER, year INTEGER)"
  runRaw conn "DROP TABLE IF EXISTS Author"
  runRaw conn "CREATE TABLE Author (authorID INTEGER PRIMARY KEY, name TEXT, address TEXT)"


  insert conn article
  putStrLn "OK"

  article' <- retrieveById conn mempty "1" :: IO Article
  print article'

  persist conn article {title = "Persistence without Boilerplate (updated)"}

  article'' <- retrieveById conn mempty "1" :: IO Article
  print article''

  print $ dropTableStmtFor (typeInfo article)
  print $ createTableStmtFor (typeInfo article)

  delete conn article''

  allArticles <- retrieveAll conn mempty :: IO [Article]
  print allArticles

  -- close connection
  disconnect conn






