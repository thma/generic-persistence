{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE DataKinds #-}

module DemoSpec
  ( test,
    spec,
  )
where

import           Database.GP
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           GHC.Generics
import           Prelude               hiding (print)
import           Test.Hspec

-- `test` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
-- (start up stack repl --test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

-- | An Entity data type with several fields, using record syntax.
data Person = Person
  { personID :: Int,
    name     :: String,
    age      :: Int,
    address  :: String
  }
  deriving (Generic, Show)

-- deriving Entity allows us to use the GenericPersistence API
-- Person is an Entity with an Int as the id type
instance Entity Person "personID"

-- print :: Show a => a -> IO ()
-- print = putStrLn . show

print :: a -> IO ()
print _ = pure ()

spec :: Spec
spec = do
  describe "A simple demo" $ do
    it "shows some basic use cases" $ do
      -- connect to a database in auto commit mode
      conn <- connect AutoCommit <$> connectSqlite3 "sqlite.db"

      -- initialize Person table
      setupTable @Person conn defaultSqliteMapping

      alice <- insert conn Person {name = "Alice", age = 25, address = "Elmstreet 1"}
      print alice

      -- update a Person
      update conn alice {address = "Main Street 200"}

      -- select a Person by id
      -- The result type must be provided by the call site,
      -- as `selectById` has a polymorphic return type `IO (Maybe a)`.
      alice' <- selectById @Person conn (personID alice)
      print alice'

      -- select all Persons from a database. again, the result type must be provided.
      allPersons <- select @Person conn allEntries
      print allPersons

      -- select all Persons from a database, where age is smaller 30.
      allPersonsUnder30 <- select @Person conn (field "age" <. (30 :: Int))
      print allPersonsUnder30

      -- delete a Person from a database
      delete conn alice

      -- select all Persons from a database. Now it should be empty.
      allPersons' <- select @Person conn allEntries
      print allPersons'

      -- close connection
      disconnect conn
