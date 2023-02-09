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
import          RIO    
import          Data.Maybe

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
  deriving (Data, Show, Eq)

data Author = Author
  { authorID :: Int,
    name     :: String,
    address  :: String
  }
  deriving (Data, Entity, Show, Eq)  

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
    it "works like a charm" $ 
      withDatabase $ do
        insert article

        author' <- retrieveById "2" :: GP (Maybe Author)
        liftIO $ author' `shouldBe` Just arthur
        
        article' <- retrieveById "1" :: GP (Maybe Article)
        liftIO $ article' `shouldBe` Just article
        



