module One2Many (main) where

import           Data.Data          
import           Database.HDBC         
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           GenericPersistence    
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

  fromRow row = local (extendCtxCache rawArticle) $ do
    maybeAuthor <- retrieveById (row !! 2) :: GP (Maybe Author)
    let author = fromMaybe (error "Author not found") maybeAuthor
    pure $ Article (col 0) (col 1) author (col 3)
    where
      col i = fromSql (row !! i)
      rawAuthor = (evidence :: Author) {authorID = col 2}
      rawArticle = Article (col 0) (col 1) rawAuthor (col 3)
    
  toRow a = do 
    persist (author a)
    return [toSql (articleID a), toSql (title a), toSql $ authorID (author a), toSql (year a)]


instance Entity Author where
  fieldsToColumns :: Author -> [(String, String)]
  fieldsToColumns _ = [("authorID", "authorID"),
                       ("name", "name"), 
                       ("address", "address")
                      ]

  fromRow row = local (extendCtxCache rawAuthor) $ do
    articlesByAuth <- retrieveAllWhere (idField rawAuthor) (idValue rawAuthor) :: GP [Article]
    pure $ rawAuthor {articles= articlesByAuth}
    where
      col i = fromSql (row !! i)
      rawAuthor = Author (col 0) (col 1) (col 2) []

  toRow a = do 
    return [toSql (authorID a), toSql (name a), toSql (address a)]

article1 :: Article
article1 = Article 
  { articleID = 1, 
    title = "Persistence without Boilerplate", 
    author = Author 
      {authorID = 1, 
      name = "Arthur Dent", 
      address = "Earth",
      articles = []}, 
    year = 2018}

article2 :: Article
article2 = Article 
  { articleID = 2, 
    title = "Boilerplate for Dummies", 
    author = Author 
      {authorID = 2, 
      name = "Arthur Miller", 
      address = "Mars Colonies",
      articles = []}, 
    year = 2020}

article3 :: Article
article3 = Article 
  { articleID = 3, 
    title = "The return of the boilerplate", 
    author = Author 
      {authorID = 2, 
      name = "Arthur Miller", 
      address = "Mars Colonies",
      articles = []}, 
    year = 2022}

arthur :: Author
arthur = Author 
  {authorID = 2, 
  name = "Arthur Miller", 
  address = "Mars Colonies", 
  articles = [article2, article3]}    

main :: IO ()
main = do
  -- connect to a database
  conn <- connectSqlite3 "sqlite.db"
  let ctx = Ctx (ConnWrapper conn) mempty 
  runRIO ctx $ do

    _ <- setupTableFor :: GP Article
    _ <- setupTableFor :: GP Author
  
    insert article1
    insert article2
    insert article3
    persist arthur
  
    article' <- retrieveById "1" :: GP(Maybe Article)
    liftIO $ print article'
  
    arthur' <- retrieveById "2" :: GP (Maybe Author)
    liftIO $ print arthur'

  -- close connection
  disconnect conn
