{-# LANGUAGE DeriveAnyClass     #-}  -- allows automatic derivation from Entity type class
module One2Many (main) where

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
    address  :: String,
    articles :: [Article]
  }
  deriving (Data, Show)  

instance Entity Article where
  fieldsToColumns :: Article -> [(String, String)]
  fieldsToColumns _ = [("articleID", "articleID"),
                       ("title", "title"), 
                       ("authorID", "authorID"),
                       ("year", "year")
                      ]

 
  fromRow :: IConnection conn => conn -> [SqlValue] -> IO Article
  fromRow conn row = do
    --author <- retrieveById conn (row !! 2) :: IO Author
    let author = Author (col 2) "" "" []
    pure $ Article (col 0) (col 1) author (col 3)
    where
      col i = fromSql (row !! i)
      
  toRow :: IConnection conn => conn -> Article -> IO [SqlValue]
  toRow conn a = do 
    persist conn (author a)
    return [toSql (articleID a), toSql (title a), toSql $ authorID (author a), toSql (year a)]

instance Entity Author where
  fieldsToColumns :: Author -> [(String, String)]
  fieldsToColumns _ = [("authorID", "authorID"),
                       ("name", "name"), 
                       ("address", "address")
                      ]

  fromRow :: IConnection conn => conn -> [SqlValue] -> IO Author
  fromRow conn row = do
    let rawAuthor = Author (col 0) (col 1) (col 2) []
    articlesByAuth <- retrieveAllWhere conn (idField rawAuthor) (idValue rawAuthor) :: IO [Article]
    pure $ rawAuthor {articles= articlesByAuth}
    where
      col i = fromSql (row !! i)
      
  toRow :: IConnection conn => conn -> Author -> IO [SqlValue]
  toRow conn a = do 
    return [toSql (authorID a), toSql (name a), toSql (address a)]

article1 :: Article
article1 = Article 
  { articleID = 1, 
    title = "Persistence without Boilerplate", 
    author = Author 
      {authorID = 1, 
      name = "Arthur Dent", 
      address = "Earth"}, 
    year = 2018}

article2 :: Article
article2 = Article 
  { articleID = 2, 
    title = "Boilerplate for Dummies", 
    author = Author 
      {authorID = 2, 
      name = "Arthur Miller", 
      address = "Mars Colonies"}, 
    year = 2020}

article3 :: Article
article3 = Article 
  { articleID = 3, 
    title = "The return of the boilerplate", 
    author = Author 
      {authorID = 2, 
      name = "Arthur Miller", 
      address = "Mars Colonies"}, 
    year = 2022}

arthur :: Author
arthur = Author 
  {authorID = 2, 
  name = "Arthur Miller", 
  address = "Mars Colonies"}    

main :: IO ()
main = do
  -- connect to a database
  conn <- connectSqlite3 "sqlite.db"

  runRaw conn "DROP TABLE IF EXISTS Article"
  runRaw conn "CREATE TABLE Article (articleID INTEGER PRIMARY KEY, title TEXT, authorID INTEGER, year INTEGER)"
  runRaw conn "DROP TABLE IF EXISTS Author"
  runRaw conn "CREATE TABLE Author (authorID INTEGER PRIMARY KEY, name TEXT, address TEXT)"


  insert conn article1
  insert conn article2
  insert conn article3
  persist conn arthur

  article' <- retrieveById conn "1" :: IO Article
  print article'

  arthur <- retrieveById conn "2" :: IO Author
  print arthur



  -- close connection
  disconnect conn






