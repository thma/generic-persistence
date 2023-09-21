-- allows automatic derivation from Entity type class
{-# LANGUAGE DeriveAnyClass #-}

module ConnSpec
  ( test,
    spec,
  )
where

import           Database.GP.GenericPersistence
import           Database.HDBC.Sqlite3
import           GHC.Generics
import           Test.Hspec
import Database.HDBC

-- `test` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery. 
-- (start up stack repl --test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

prepareDB :: IO Conn
prepareDB = do
  conn <- connect SQLite <$> connectSqlite3 ":memory:"
  setupTableFor @Article conn
  return conn

data Article = Article
  { articleID :: Int,
    title     :: String,
    year      :: Int
  }
  deriving (Generic, Entity, Show, Eq)

spec :: Spec
spec = do
  describe "Connection Handling" $ do
    it "can work with embedded connection" $ do
      Conn{db=db',implicitCommit=ic, connection=conn} <- prepareDB
      db' `shouldBe` SQLite
      show db' `shouldBe` "SQLite"
      ic `shouldBe` True
      
      runRaw conn "DROP TABLE IF EXISTS Person;"
      let conn' = connect SQLite conn

      allArticles <- select conn' allEntries :: IO [Article]
      allArticles `shouldBe` []

    it "can handle rollback" $ do
      conn <- prepareDB
      let conn' = conn{implicitCommit=False}
      let article = Article 1 "Hello" 2023
      
      insert conn' article
      allArticles <- select conn' allEntries :: IO [Article]
      allArticles `shouldBe` [article]
      rollback conn'
      allArticles' <- select conn' allEntries :: IO [Article]
      allArticles' `shouldBe` []

    it "provide the IConnection methods" $ do
      conn <- prepareDB
      getTables conn `shouldReturn` ["Article"]

      desc <- describeTable conn "Article" 
      length desc `shouldBe` 3

      let driverName = hdbcDriverName conn
      driverName `shouldBe` "sqlite3"

      let clientVer = hdbcClientVer conn
      head clientVer `shouldBe` '3'

      let proxiedClient = proxiedClientName conn
      proxiedClient `shouldBe` "sqlite3"

      let proxiedClientVerion = proxiedClientVer conn
      head proxiedClientVerion `shouldBe` '3'

      let serverVer = dbServerVer conn
      head serverVer `shouldBe` '3'

      let txSupport = dbTransactionSupport conn
      txSupport `shouldBe` True

      clonedConn <- clone conn
      db clonedConn `shouldBe` db conn




      
      
      


