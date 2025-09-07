module ReferenceSpec
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
-- (start up stack repl --test to bring up ghci and have access to all the test functions)
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
    author    :: Author,
    year      :: Int
  }
  deriving (Generic, Show, Eq)

data Author = Author
  { authorID :: Int,
    name     :: String,
    address  :: String
  }
  deriving (Generic, Show, Eq)

instance Entity Author "authorID" Int where
  autoIncrement = False

instance Entity Article "articleID" Int where
  autoIncrement :: Bool
  autoIncrement = False

  fieldsToColumns :: [(String, String)] -- ommitting the author field,
  fieldsToColumns =
    -- as this can not be mapped to a single column
    [ ("articleID", "articleID"), -- instead we invent a new column authorID
      ("title", "title"),
      ("authorID", "authorID"),
      ("year", "year")
    ]

  fromRow :: Conn -> [SqlValue] -> IO Article
  fromRow conn row = do
    let fkValue = fromSql (atIndex 2) :: Int
    maybeAuthor <- selectById conn fkValue -- load author by foreign key
    let authorById = case maybeAuthor of
                       Just a  -> a
                       Nothing -> error $ "fromRow: author not found for id " ++ show fkValue
    return $ rawArticle {author = authorById} -- add author to article
    where
      atIndex n = case drop n row of
                    (y:_) -> y
                    []    -> error $ "fromRow: index " ++ show n ++ " out of bounds"
      rawArticle =
        Article
          (col 0)
          (col 1) -- create article from row,
          (Author (col 2) "" "")
          (col 3) -- using a dummy author
        where
          col i = fromSql (atIndex i)

  toRow :: Conn -> Article -> IO [SqlValue]
  toRow conn a = do
    upsert conn (author a) -- persist author first
    return
      [ toSql (articleID a),
        toSql (title a), -- return row for article table where
        toSql $ authorID (author a),
        toSql (year a) -- authorID is foreign key to author table
      ]

article :: Article
article =
  Article
    { articleID = 1,
      title = "Persistence without Boilerplate",
      author = arthur,
      year = 2018
    }

arthur :: Author
arthur =
  Author
    { authorID = 2,
      name = "Arthur Miller",
      address = "Denver"
    }

spec :: Spec
spec = do
  describe "Handling of 1:1 References" $ do
    it "works like a charm" $ do
      conn <- prepareDB
      _ <- insert conn article

      author' <- selectById conn (2::Int) :: IO (Maybe Author)
      author' `shouldBe` Just arthur

      article' <- selectById conn (1::Int) :: IO (Maybe Article)
      article' `shouldBe` Just article
