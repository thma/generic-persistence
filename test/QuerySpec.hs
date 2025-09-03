module QuerySpec
  ( test,
    spec,
  )
where

import           Database.GP
import           Database.GP.SqlGenerator
import           Database.HDBC.Sqlite3    (connectSqlite3)
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
  setupTable @Person conn defaultSqliteMapping

  insertMany conn [alice, bob, charlie]
  return conn

-- | A data type with several fields, using record syntax.
data Person = Person
  { personID :: Int,
    name     :: String,
    age      :: Int,
    address  :: String
  }
  deriving (Generic, Show, Eq)

instance Entity Person "personID" Int where
  autoIncrement = False

alice, bob, charlie, dave :: Person
bob = Person 1 "Bob" 36 "West Street 79"
alice = Person 2 "Alice" 25 "West Street 90"
charlie = Person 3 "Charlie" 35 "West Street 40"
dave = Person 4 "Dave" 35 "East Street 12"

nameField, ageField, addressField :: Field
nameField = field "name"
ageField = field "age"
addressField = field "address"

lower :: Field -> Field
lower = sqlFun "LOWER"

upper :: Field -> Field
upper = sqlFun "UPPER"

spec :: Spec
spec = do
  describe "The Query DSL against SQLite" $ do
    it "supports conjunction with &&." $ do
      conn <- prepareDB
      one <- select conn (nameField =. "Bob" &&. ageField =. (36 :: Int))
      length one `shouldBe` 1
      case one of
        (p:_) -> p `shouldBe` bob
        []    -> expectationFailure "one is empty"
    it "supports disjunction with ||." $ do
      conn <- prepareDB
      two <- select conn (nameField =. "Bob" ||. ageField =. (25 :: Int))
      length two `shouldBe` 2
      two `shouldContain` [bob, alice]
    it "supports LIKE" $ do
      conn <- prepareDB
      three <- select conn (addressField `like` "West Street %") :: IO [Person]
      length three `shouldBe` 3
    it "supports NOT" $ do
      conn <- prepareDB
      empty <- select conn (not' $ addressField `like` "West Street %") :: IO [Person]
      length empty `shouldBe` 0
    it "supports fieldwise comparisons like >" $ do
      conn <- prepareDB
      boomers <- select conn (ageField >. (30 :: Int))
      length boomers `shouldBe` 2
      boomers `shouldContain` [bob, charlie]
      teens <- select conn (ageField <. (20 :: Int)) :: IO [Person]
      length teens `shouldBe` 0
      wisePerson <- select conn (ageField >=. (50 :: Int)) :: IO [Person]
      length wisePerson `shouldBe` 0
      notAbove25 <- select conn (ageField <=. (25 :: Int)) :: IO [Person]
      length notAbove25 `shouldBe` 1
      allButBob <- select conn (ageField <>. (36 :: Int)) :: IO [Person]
      length allButBob `shouldBe` 2
    it "supports BETWEEN" $ do
      conn <- prepareDB
      thirtySomethings <- select conn (ageField `between` (30 :: Int, 40 :: Int)) :: IO [Person]
      length thirtySomethings `shouldBe` 2
      thirtySomethings `shouldContain` [bob, charlie]
    it "supports IN" $ do
      conn <- prepareDB
      aliceAndCharlie <- select conn (nameField `in'` ["Alice", "Charlie"])
      length aliceAndCharlie `shouldBe` 2
      aliceAndCharlie `shouldContain` [alice, charlie]
    it "supports IS NULL" $ do
      conn <- prepareDB
      noOne <- select conn (isNull nameField) :: IO [Person]
      length noOne `shouldBe` 0
    it "supports IS NOT NULL" $ do
      conn <- prepareDB
      allPersons <- select conn (not' $ isNull nameField) :: IO [Person]
      length allPersons `shouldBe` 3
    it "supports SQL functions on columns" $ do
      conn <- prepareDB
      peopleFromWestStreet <- select conn (lower (upper addressField) `like` "west street %") :: IO [Person]
      length peopleFromWestStreet `shouldBe` 3
    it "supports selection by id" $ do
      conn <- prepareDB
      charlie' <- select conn (byId "3") :: IO [Person]
      length charlie' `shouldBe` 1
      case charlie' of
        (p:_) -> p `shouldBe` charlie
        []    -> expectationFailure "charlie' is empty"
    it "supports ORDER BY" $ do
      conn <- prepareDB
      sortedPersons <- select @Person conn (allEntries `orderBy` (ageField, ASC) :| [])
      length sortedPersons `shouldBe` 3
      sortedPersons `shouldBe` [alice, charlie, bob]
    it "supports multiple columns in ORDER BY" $ do
      conn <- prepareDB
      _ <- insert conn dave -- dave and charlie have the same age
      sortedPersons <- select @Person conn (allEntries `orderBy` (ageField, ASC) :| [(nameField, DESC)])
      length sortedPersons `shouldBe` 4
      sortedPersons `shouldBe` [alice, dave, charlie, bob]
    it "supports LIMIT" $ do
      conn <- prepareDB
      _ <- insert conn dave
      limitedPersons <- select @Person conn (allEntries `limit` 2)
      length limitedPersons `shouldBe` 2
    it "supports LIMIT OFFSET" $ do
      conn <- prepareDB
      _ <- insert conn dave
      limitedPersons <- select @Person conn (allEntries `limitOffset` (2, 1))
      length limitedPersons `shouldBe` 1
      case limitedPersons of
        (p:_) -> p `shouldBe` charlie
        []    -> expectationFailure "limitedPersons is empty"
    it "can create column types for a SqlLite" $ do
      columnTypeFor @SomeRecord defaultSqliteMapping "someRecordID" `shouldBe` "INTEGER"
      columnTypeFor @SomeRecord defaultSqliteMapping "someRecordName" `shouldBe` "TEXT"
      columnTypeFor @SomeRecord defaultSqliteMapping "someRecordAge" `shouldBe` "REAL"
      columnTypeFor @SomeRecord defaultSqliteMapping "someRecordTax" `shouldBe` "REAL"
      columnTypeFor @SomeRecord defaultSqliteMapping "someRecordFlag" `shouldBe` "INT"
      columnTypeFor @SomeRecord defaultSqliteMapping "someRecordDate" `shouldBe` "TEXT"
    it "can create column types for a Postgres" $ do
      columnTypeFor @SomeRecord defaultPostgresMapping "someRecordID" `shouldBe` "serial"
      columnTypeFor @SomeRecord defaultPostgresMapping "someRecordName" `shouldBe` "varchar"
      columnTypeFor @SomeRecord defaultPostgresMapping "someRecordAge" `shouldBe` "numeric"
      columnTypeFor @SomeRecord defaultPostgresMapping "someRecordTax" `shouldBe` "numeric"
      columnTypeFor @SomeRecord defaultPostgresMapping "someRecordFlag" `shouldBe` "boolean"
      columnTypeFor @SomeRecord defaultPostgresMapping "someRecordDate" `shouldBe` "varchar"
    it "can create whereclauses" $ do
      whereClauseValues byIdColumn `shouldBe` []
    it "allows to write custom SQL queries" $ do
      conn <- prepareDB
      _ <- insert conn dave
      let stmt =
            [sql|
        SELECT *
        FROM person
        WHERE name = (?)|]
      persons <- entitiesFromRows @Person conn =<< quickQuery conn stmt [toSql "Dave"]
      length persons `shouldBe` 1
      case persons of
        (p:_) -> p `shouldBe` dave
        []    -> expectationFailure "persons is empty"

data SomeRecord = SomeRecord
  { someRecordID   :: Int,
    someRecordName :: String,
    someRecordAge  :: Double,
    someRecordTax  :: Float,
    someRecordFlag :: Bool,
    someRecordDate :: Integer
  }
  deriving (Generic, Show, Eq)

instance Entity SomeRecord "someRecordID" Int
