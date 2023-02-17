{-# LANGUAGE DeriveAnyClass     #-}  -- allows automatic derivation from Entity type class
module OneToManySpec
  ( test
  , spec
  ) where

import          Test.Hspec
import          Data.Data
import          Database.HDBC
import          Database.HDBC.Sqlite3
import          Database.GP.GenericPersistence
import          RIO    
import          Data.Maybe (fromJust)
import          GHC.Generics

-- `test` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery. (start up stack repl --test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

withDatabase :: RIO Ctx a -> IO a
withDatabase action = do
  conn <- connectSqlite3 ":memory:"
  runGP conn $ do
    _ <- setupTableFor :: GP Article
    _ <- setupTableFor :: GP Author
    action

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
    address  :: String,
    articles :: [Article]
  }
  deriving (Generic, Data, Show, Eq)  

instance Entity Article where
  fieldsToColumns :: Article -> [(String, String)]
  fieldsToColumns _ = [("articleID", "articleID"),
                       ("title", "title"), 
                       ("authorID", "authorID"),
                       ("year", "year")
                      ]

  fromRow :: [SqlValue] -> GP Article
  fromRow row = local (extendCtxCache rawArticle) $ do
    maybeAuthor <- getElseRetrieve (entityId rawAuthor)
    let author = fromJust maybeAuthor
    pure $ Article (col 0) (col 1) author (col 3)
    where
      col i = fromSql (row !! i)
      rawAuthor = (evidence :: Author) {authorID = col 2}
      rawArticle = Article (col 0) (col 1) rawAuthor (col 3)
    
  toRow a = do 
    persist (author a)
    return $ toRowWoCtx a --[toSql (articleID a), toSql (title a), toSql $ authorID (author a), toSql (year a)]

  toRowWoCtx a = [toSql (articleID a), toSql (title a), toSql $ authorID (author a), toSql (year a)]


instance Entity Author where
  fieldsToColumns :: Author -> [(String, String)]
  fieldsToColumns _ = [("authorID", "authorID"),
                       ("name", "name"), 
                       ("address", "address")
                      ]

  fromRow :: [SqlValue] -> GP Author
  fromRow row = local (extendCtxCache rawAuthor) $ do
    articlesByAuth <- retrieveAllWhere (idField rawAuthor) (idValue rawAuthor) :: GP [Article]
    pure $ rawAuthor {articles= articlesByAuth}
    where
      col i = fromSql (row !! i)
      rawAuthor = Author (col 0) (col 1) (col 2) []

  toRow :: Author -> GP [SqlValue]
  toRow = return . toRowWoCtx

  toRowWoCtx a = [toSql (authorID a), toSql (name a), toSql (address a)]

article1 :: Article
article1 = Article 
  { articleID = 1, 
    title = "Persistence without Boilerplate", 
    author = Author 
      {authorID = 1, 
      name = "Max Millian", 
      address = "Boston",
      articles = []}, 
    year = 2018}

article2 :: Article
article2 = Article 
  { articleID = 2, 
    title = "Boilerplate for Dummies", 
    author = arthur, 
    year = 2020}

article3 :: Article
article3 = Article 
  { articleID = 3, 
    title = "The return of the boilerplate", 
    author = arthur, 
    year = 2022}

arthur :: Author
arthur = Author 
  {authorID = 2, 
  name = "Arthur Miller", 
  address = "Denver", 
  articles = [article2, article3]}    

spec :: Spec
spec = do
  describe "Handling of 1:N References" $ do
    it "works like a charm" $ 
      withDatabase $ do
        insert article1
        insert article2
        insert article3

        authors <- retrieveAll :: GP [Author]
        liftIO $ length authors `shouldBe` 2
        --liftIO $ print authors
        articles <- retrieveAll :: GP [Article]
        liftIO $ length articles `shouldBe` 3
        
        article' <- retrieveById "3" :: GP (Maybe Article)
        let art = fromJust article'
        liftIO $ (name (author art)) `shouldBe` "Arthur Miller" 
        
        



