{-# LANGUAGE DeriveAnyClass #-}
module QuerySpec 
( test
, spec
)
where 

import           Database.GP
import Database.HDBC.Sqlite3 ( connectSqlite3 )
import           GHC.Generics
import           Test.Hspec

-- `test` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery. 
-- (start up stack repl --test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

prepareDB :: IO Conn
prepareDB = do
  conn <- connect SQLite <$> connectSqlite3 ":memory:"
  setupTableFor @Person conn

  insertMany conn [alice, bob, charlie]
  return conn

-- | A data type with several fields, using record syntax.
data Person = Person
  { personID :: Int,
    name     :: String,
    age      :: Int,
    address  :: String
  }
  deriving (Generic, Entity, Show, Eq)

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
lower = sqlFun "LOWER";

upper :: Field -> Field
upper = sqlFun "UPPER";

spec :: Spec
spec = do
  describe "The Query DSL" $ do
    it "supports conjunction with &&." $ do
      conn <- prepareDB
      one <- select conn (nameField =. "Bob" &&. ageField =. (36 :: Int))
      length one `shouldBe` 1
      head one `shouldBe` bob
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
      peopleFromWestStreet <- select conn (lower(upper addressField) `like` "west street %") :: IO [Person]
      length peopleFromWestStreet `shouldBe` 3
    it "supports selection by id" $ do
      conn <- prepareDB
      charlie' <- select conn (byId "3") :: IO [Person]
      length charlie' `shouldBe` 1
      head charlie' `shouldBe` charlie
    it "supports ORDER BY" $ do
      conn <- prepareDB
      sortedPersons <- select @Person conn (allEntries `orderBy` [(ageField,ASC)])
      length sortedPersons `shouldBe` 3
      sortedPersons `shouldBe` [alice, charlie, bob]
    it "supports multiple columns in ORDER BY" $ do
      conn <- prepareDB
      insert conn dave -- dave and charlie have the same age
      sortedPersons <- select @Person conn (allEntries `orderBy` [(ageField,ASC), (nameField,DESC)])
      length sortedPersons `shouldBe` 4
      sortedPersons `shouldBe` [alice, dave, charlie, bob]
    it "supports LIMIT" $ do
      conn <- prepareDB
      insert conn dave
      limitedPersons <- select @Person conn (allEntries `limit` 2)
      length limitedPersons `shouldBe` 2
    it "supports LIMIT OFFSET" $ do
      conn <- prepareDB
      insert conn dave
      limitedPersons <- select @Person conn (allEntries `limitOffset` (2,1))
      length limitedPersons `shouldBe` 1
      head limitedPersons `shouldBe` charlie