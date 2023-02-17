module EmbeddedSpec
  ( test
  , spec
  ) where

import          Test.Hspec
import          Data.Data
import          Database.HDBC
import          Database.HDBC.Sqlite3
import          Database.GP.GenericPersistence
import          RIO    
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
    address  :: String
  }
  deriving (Generic, Data, Show, Eq)  

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

  toRowWoCtx  a = [toSql (articleID a), toSql (title a), toSql authID, toSql authorName, toSql authorAddress, toSql (year a)]
    where 
      authID = authorID (author a)
      authorName = name (author a)
      authorAddress = address (author a)

  toRow = return . toRowWoCtx

article :: Article
article = Article 
  { articleID = 1, 
    title = "Persistence without Boilerplate", 
    author = Author 
      {authorID = 1, 
      name = "Arthur Dent", 
      address = "Boston"}, 
    year = 2018}

spec :: Spec
spec = do
  describe "Handling of Embedded Objects" $ do
    it "works like a charm" $ 
      withDatabase $ do
        insert article
        article' <- retrieveById "1" :: GP (Maybe Article)
        liftIO $ article' `shouldBe` Just article
        allArticles <- retrieveAll :: GP [Article]
        liftIO $ allArticles `shouldBe` [article]


