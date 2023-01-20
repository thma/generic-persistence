{-# LANGUAGE DeriveDataTypeable#-}
module Main (main, main1) where

import Data.Data ( Data )
import TypeInfo ( gshow ) 
import GenericPersistence
import Database.HDBC (disconnect, runRaw, commit) 
import Database.HDBC.Sqlite3 ( connectSqlite3 )


-- | A data type with several fields, using record syntax.
data Person = Person
  { personID :: Int
  , name :: String
  , age :: Int
  , address :: String
  } deriving (Data)

main :: IO ()
main = do
    -- initialize Person table
    conn <- connectSqlite3 "sqlite.db"
    runRaw conn "DROP TABLE IF EXISTS Person;"
    runRaw conn "CREATE TABLE IF NOT EXISTS Person (personID INT PRIMARY KEY, name TEXT, age INT, address TEXT);"
    commit conn
  
    -- create a Person entity
    let alice = Person {personID = 123456, name = "Alice", age = 25, address = "Elmstreet 1"}

    -- insert a Person into a database
    persist conn alice

    -- update a Person
    persist conn alice {address = "Main Street 200"}  
    
    -- select a Person from a database
    -- The result type must be provided explicitly, as `retrieveEntityById` has a polymorphic return type `IO a`.
    alice' <- retrieveById conn "123456" :: IO Person

    -- delete a Person from a database
    delete conn alice'
    
    -- close connection
    disconnect conn


p :: Person
p = Person 123456 "Alice" 25 "123 Main St"

main1 :: IO ()
main1 = do

  -- initialize Person table
  conn <- connectSqlite3 "sqlite.db"
  runRaw conn "DROP TABLE IF EXISTS Person;"
  runRaw conn "CREATE TABLE IF NOT EXISTS Person (personID INT PRIMARY KEY, name TEXT, age INT, address TEXT);"
  commit conn
  
  -- insert a Person into a database
  persist conn p
  
  -- insert a second Person in a database
  persist conn p {personID = 123457, name = "Bob"}
  
  -- update a Person
  persist conn p {address = "Elmstreet 1"}  
  
  -- select a Person from a database
  alice <- retrieveById conn (123456 :: Int) :: IO Person
  print $ gshow alice

  -- select all Persons from a database
  allPersons <- retrieveAll conn :: IO [Person]
  print $ gshow allPersons

  -- delete a Person from a database
  delete conn alice
  
  -- select all Persons from a database
  allPersons' <- retrieveAll conn :: IO [Person]
  print $ gshow allPersons'

  -- close connection
  disconnect conn


