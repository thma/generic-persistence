{-# LANGUAGE DeriveDataTypeable#-}
--{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications#-}
{-# LANGUAGE AllowAmbiguousTypes#-}
module Main (main) where

import Data.Data hiding (typeRep, IntRep, TypeRep)

import TypeInfo
--import RecordtypeReflection
--import SqlGenerator
import DataPersistence
--import Data.Dynamic
--import Type.Reflection
--import GHC.Data.Maybe (expectJust)
--import Data.Maybe (fromMaybe)
--import Control.Monad

import Database.HDBC
import Database.HDBC.Sqlite3


-- | A data type with several fields, using record syntax.
data Person = Person
  { personID :: !Int
  , name :: !String
  , age :: !Int
  , address :: !String
  } deriving (Data, Show)




main :: IO ()
main = do
  let p = Person 123456 "Alice" 25 "123 Main St"
  -- generate sql statements for a Person p
  --putStrLn $ insertStmtFor p
  --putStrLn $ selectStmtFor (typeInfo p) "123456"
  --putStrLn $ updateStmtFor p
  --putStrLn $ deleteStmtFor p

  -- initialize Person table
  conn <- connectSqlite3 "sqlite1.db"
  runRaw conn ("DROP TABLE IF EXISTS Person;")
  runRaw conn ("CREATE TABLE IF NOT EXISTS Person (personID INT PRIMARY KEY, name TEXT, age INT, address TEXT);")
  commit conn
  
  -- insert a Person into a database
  persistEntity conn p
  
  -- update a Person in a database
  persistEntity conn p {name = "Bob"}
  
  -- select a Person from a database
  entity <- retrieveEntityById conn (typeInfo p) "123456" :: IO Person
  print entity
  
  -- delete a Person from a database
  deleteEntity conn p
  
  -- insert a Person into a database
  persistEntity conn p
    
  entity' <- retrieveEntityById conn (typeInfo p) "123456" :: IO Person
  print entity'
  

  
  disconnect conn


