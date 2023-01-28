{-# LANGUAGE DeriveAnyClass     #-}  -- allows automatic derivation from Entity type class
module Reference (main) where

import           Data.Data             (Data)
import           Database.HDBC         
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           GenericPersistence    
import SqlGenerator (createTableStmtFor, dropTableStmtFor)
import TypeInfo

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

  fromRow :: [SqlValue] -> Article
  fromRow row = Article (col 0) (col 1) author (col 3)
    where
      col i = fromSql (row !! i)
      author = undefined --retrieveById conn (col 2) :: Author

  toRow :: Article -> [SqlValue]
  toRow a = [toSql (articleID a), toSql (title a), toSql authID, toSql authorName, toSql authorAddress, toSql (year a)]
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
      address = "Earth"}, 
    year = 2018}

main :: IO ()
main = do
  -- connect to a database
  conn <- connectSqlite3 "sqlite.db"

  runRaw conn "DROP TABLE IF EXISTS Article"
  runRaw conn "CREATE TABLE Article (articleID INTEGER PRIMARY KEY, title TEXT, authorID INTEGER, authorName TEXT, authorAddress TEXT, year INTEGER)"

  insert conn article

  article' <- retrieveById conn "1" :: IO Article
  print article'

  persist conn article {title = "Persistence without Boilerplate (updated)"}

  article'' <- retrieveById conn "1" :: IO Article
  print article''

  print $ dropTableStmtFor (typeInfo article)
  print $ createTableStmtFor (typeInfo article)

  delete conn article''

  allArticles <- retrieveAll conn :: IO [Article]
  print allArticles

  -- close connection
  disconnect conn






