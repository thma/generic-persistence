-- this compiler pragma allows GHC to automatically discover all Hspec Test Specs.
--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
-- Avoid warning for missing export list in test/Spec.hs
--{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Spec where

import           Database.GP
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           GHC.Generics

-- | An Entity data type with several fields, using record syntax.
data Person = Person
  { personID :: Int,
    name     :: String,
    age      :: Int,
    address  :: String
  }
  deriving (Generic, Entity, Show) -- deriving Entity allows us to use the GenericPersistence API

print :: a -> IO ()
print _ = pure ()

main :: IO ()
main = do
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

