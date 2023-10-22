module ConnSpec
  ( test,
    spec,
  )
where

import           Database.GP
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
      (Conn ic conn) <- prepareDB
      ic `shouldBe` True

      runRaw conn "DROP TABLE IF EXISTS Person;"
      let conn' = connect AutoCommit conn

      allArticles <- select conn' allEntries :: IO [Article]
      allArticles `shouldBe` []

    it "can handle rollback" $ do
      conn <- prepareDB
      let conn' = connect ExplicitCommit conn
      let article = Article 1 "Hello" 2023

      _ <- insert conn' article
      allArticles <- select conn' allEntries :: IO [Article]
      allArticles `shouldBe` [article]
      rollback conn'
      allArticles' <- select conn' allEntries :: IO [Article]
      allArticles' `shouldBe` []

    it "provide the IConnection methods" $ do
      conn <- prepareDB
      allTables <- getTables conn
      allTables `shouldContain` ["Article"]

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

      let (Conn autoCommit _) = conn
      (Conn clonedAutoCommit _) <- clone conn
      clonedAutoCommit `shouldBe` autoCommit
