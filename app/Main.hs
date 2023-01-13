{-# LANGUAGE DeriveDataTypeable#-}
{-# LANGUAGE ExtendedDefaultRules#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications#-}
{-# LANGUAGE AllowAmbiguousTypes#-}
module Main (main) where

import Data.Data hiding (typeRep, IntRep, TypeRep)

import TypeInfo
import RecordtypeReflection hiding (convert)
import SqlGenerator
import Data.Dynamic
import Type.Reflection
import GHC.Data.Maybe (expectJust)
import Data.Maybe (fromMaybe)
import Control.Monad

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Convertible.Base (convert, safeConvert)

-- | A data type with several fields, using record syntax.
data Person = Person
  { id :: !Int
  , name :: !String
  , age :: !Int
  , address :: !String
  } deriving (Data, Show)

p = Person 123456 "Alice" 25 "123 Main St"


main :: IO ()
main = do
  -- generate sql statements for a Person p
  putStrLn $ insertStmtFor p
  putStrLn $ selectStmtFor (typeInfo p) "123456"
  putStrLn $ updateStmtFor p
  putStrLn $ deleteStmtFor p

  -- initialize Person table
  conn <- connectSqlite3 "sqlite1.db"
  run conn ("DROP TABLE IF EXISTS Person;") []
  run conn ("CREATE TABLE IF NOT EXISTS Person (id INT PRIMARY KEY, name TEXT, age INT, address TEXT);") []
  commit conn
  
  -- insert a Person into a database
  run conn (insertStmtFor p) []
  commit conn
  
  -- select a Person from a database
  entity <- retrieveEntity conn "123456" (typeInfo p) :: IO Person
  print entity
  
  disconnect conn


-- | A function that retrieves an entity from a database.
-- I would like to get rid of the TypeInfo paraemeter and derive it directly from the 'IO a' result type.
-- This will need some helping hand from the Internet...
retrieveEntity :: forall a. (Data a) => Connection -> String -> TypeInfo -> IO a
retrieveEntity conn id ti = do
  let stmt = selectStmtFor ti id
  resultRowsSqlValues <- quickQuery conn stmt []
  let (resultRow :: [String]) = map convert (head resultRowsSqlValues)
  return $ expectJust ("No " ++ (show $ typeName ti) ++ " found for id " ++ id) (buildFromRecord ti resultRow :: Maybe a)
