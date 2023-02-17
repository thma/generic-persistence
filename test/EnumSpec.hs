{-# LANGUAGE DeriveAnyClass #-}
module EnumSpec
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
    _ <- setupTableFor :: GP Book
    action

data Book = Book
  { bookID :: Int,
    title   :: String,
    author  :: String,
    year    :: Int,
    category :: BookCategory
  }
  deriving (Generic, Data, Show, Eq)

data BookCategory = Fiction | Travel | Arts | Science | History | Biography | Other
  deriving (Generic, Data, Show, Read, Eq, Enum)
  
instance Entity Book where
  fromRow row = return $ Book (col 0) (col 1) (col 2) (col 3) (col 4)
    where
      col i = fromSql (row !! i)

  toRow b = return [toSql (bookID b), toSql (title b), toSql (author b), toSql (year b), toSql (category b)]

-- instance Convertible BookCategory SqlValue where
--   safeConvert = Right . toSql . show
  
-- instance Convertible SqlValue BookCategory where
--   safeConvert = Right . read . fromSql

spec :: Spec
spec = do
  describe "Handling of Enum Fields" $ do
    it "works like a charm" $ 
      withDatabase $ do
        let book = Book 1 "The Hobbit" "J.R.R. Tolkien" 1937 Fiction
        insert book
        allBooks <- retrieveAll :: GP [Book]
        liftIO $ allBooks `shouldBe` [book]


