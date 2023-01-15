{-# LANGUAGE DeriveDataTypeable#-}
module Main (main) where

import Data.Data ( Data )
import TypeInfo ( typeInfo ) 
import GenericPersistence
    ( deleteEntity,
      persistEntity,
      retrieveAllEntities,
      retrieveEntityById )
import Database.HDBC (disconnect, runRaw, commit) 
import Database.HDBC.Sqlite3 ( connectSqlite3 )


-- | A data type with several fields, using record syntax.
data Person = Person
  { personID :: !Int
  , name :: !String
  , age :: !Int
  , address :: !String
  } deriving (Data, Show)

p :: Person
p = Person 123456 "Alice" 25 "123 Main St"

main :: IO ()
main = do
  -- generate sql statements for a Person p
  --putStrLn $ insertStmtFor p
  --putStrLn $ selectStmtFor (typeInfo p) "123456"
  --putStrLn $ updateStmtFor p
  --putStrLn $ deleteStmtFor p

  -- initialize Person table
  conn <- connectSqlite3 "sqlite.db"
  runRaw conn "DROP TABLE IF EXISTS Person;"
  runRaw conn "CREATE TABLE IF NOT EXISTS Person (personID INT PRIMARY KEY, name TEXT, age INT, address TEXT);"
  commit conn
  
  -- insert a Person into a database
  persistEntity conn p
  
  -- insert a second Person in a database
  persistEntity conn p {personID = 123457, name = "Bob"}
  
  -- update a Person
  persistEntity conn p {address = "Elmstreet 1"}  
  
  -- select a Person from a database
  alice <- retrieveEntityById conn (typeInfo p) (123456 :: Int) :: IO Person
  print alice

  -- select all Persons from a database
  allPersons <- retrieveAllEntities conn (typeInfo p) :: IO [Person]
  print allPersons

  -- delete a Person from a database
  deleteEntity conn alice
  
  -- select all Persons from a database
  allPersons' <- retrieveAllEntities conn (typeInfo p) :: IO [Person]
  print allPersons'

  -- close connection
  disconnect conn


