module OneToManySpec
  ( test,
    spec,
  )
where

import           Database.GP.GenericPersistence
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           GHC.Generics
import           Test.Hspec

-- `test` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
-- (start up stack repl -- test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

prepareDB :: IO Conn
prepareDB = do
  conn <- connect AutoCommit <$> connectSqlite3 ":memory:"
  setupTable @Article conn defaultSqliteMapping
  setupTable @Author conn defaultSqliteMapping
  return conn

data Article = Article
  { articleID :: Int,
    title     :: String,
    authorId  :: Int,
    year      :: Int
  }
  deriving (Generic, Show, Eq)

instance Entity Article "articleID" Int where
  autoIncrement = False

data Author = Author
  { authorID :: Int,
    name     :: String,
    address  :: String,
    articles :: [Article]
  }
  deriving (Generic, Show, Eq)

instance Entity Author "authorID" Int where
  fieldsToColumns :: [(String, String)] -- ommitting the articles field,
  fieldsToColumns =
    -- as this can not be mapped to a single column
    [ ("authorID", "authorID"),
      ("name", "name"),
      ("address", "address")
    ]
  autoIncrement = False

  fromRow :: Conn -> [SqlValue] -> IO Author
  fromRow conn row = do
    let authID = case row of
                   (x:_) -> x
                   []    -> error "fromRow: empty row" -- This should never happen in practice
    articlesBy <- select conn (field "authorId" =. authID) -- retrieve all articles by this author
    return rawAuthor {articles = articlesBy} -- add the articles to the author
    where
      rawAuthor = Author (col 0) (col 1) (col 2) [] -- create the author from row (w/o articles)
      col i = fromSql (atIndex i) -- helper function to convert SqlValue to Haskell type
      atIndex n = case drop n row of
                    (y:_) -> y
                    []    -> error $ "fromRow: index " ++ show n ++ " out of bounds"

  toRow :: Conn -> Author -> IO [SqlValue]
  toRow conn a = do
    mapM_ (upsert conn) (articles a) -- persist all articles of this author (update or insert)
    return
      [ toSql (authorID a), -- return the author as a list of SqlValues
        toSql (name a),
        toSql (address a)
      ]

article1 :: Article
article1 =
  Article
    { articleID = 1,
      title = "Persistence without Boilerplate",
      authorId = 1,
      year = 2018
    }

article2 :: Article
article2 =
  Article
    { articleID = 2,
      title = "Boilerplate for Dummies",
      authorId = 2,
      year = 2020
    }

article3 :: Article
article3 =
  Article
    { articleID = 3,
      title = "The return of the boilerplate",
      authorId = 2,
      year = 2022
    }

arthur :: Author
arthur =
  Author
    { authorID = 2,
      name = "Arthur Miller",
      address = "Denver",
      articles = [article2, article3]
    }

spec :: Spec
spec = do
  describe "Handling of 1:N References" $ do
    it "works like a charm" $ do
      conn <- prepareDB

      _ <- insert conn arthur
      _ <- insert conn article1

      authors <- select conn allEntries :: IO [Author]
      length authors `shouldBe` 1

      articles' <- select conn allEntries :: IO [Article]
      length articles' `shouldBe` 3

      author2 <- selectById conn (2::Int) :: IO (Maybe Author)
      case author2 of
        Just auth -> do
          auth `shouldBe` arthur
          length (articles auth) `shouldBe` 2
        Nothing -> expectationFailure "author2 is Nothing"
