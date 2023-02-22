{-# LANGUAGE DeriveAnyClass #-}

module EnumSpec
  ( test,
    spec,
  )
where

import           Data.Convertible
import           Database.GP.GenericPersistence
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           GHC.Generics
import           Test.Hspec

-- `test` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery. (start up stack repl --test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

prepareDB :: IO Conn
prepareDB = do
  conn <- Conn SQLite <$> connectSqlite3 ":memory:"
  _ <- setupTableFor conn :: IO Book
  return conn

data Book = Book
  { bookID   :: Int,
    title    :: String,
    author   :: String,
    year     :: Int,
    category :: BookCategory
  }
  deriving (Generic, Show, Eq, Entity)

data BookCategory = Fiction | Travel | Arts | Science | History | Biography | Other
  deriving (Generic, Show, Read, Eq)

instance Convertible BookCategory SqlValue where
  safeConvert = Right . toSql . show

instance Convertible SqlValue BookCategory where
  safeConvert = Right . read . fromSql

spec :: Spec
spec = do
  describe "Handling of Enum Fields" $ do
    it "works like a charm" $ do
      conn <- prepareDB
      let book = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937 Fiction
      insert conn book
      allBooks <- retrieveAll conn :: IO [Book]
      allBooks `shouldBe` [book]
