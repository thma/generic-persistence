{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE QuantifiedConstraints#-}
module DataPersistence
  (
    retrieveEntity
  ,
  )
where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Convertible.Base (convert, safeConvert)
import TypeInfo
import Data.Data
import RecordtypeReflection
import SqlGenerator
import GHC.Data.Maybe (expectJust)
import Data.Maybe (fromMaybe)


{--
 This module defines RDBMS Persistence operations for Record Data Types that are instances of 'Data'.
 The Persistence operations are using Haskell generics to provide compile time reflection capabilities.
 HDBC is used to access the RDBMS.
--}


-- | A function that retrieves an entity from a database.
-- I would like to get rid of the TypeInfo paraemeter and derive it directly from the 'IO a' result type.
-- This will need some helping hand from the Internet...
retrieveEntity :: forall a. Data a => Connection -> String -> TypeInfo -> IO a
retrieveEntity conn id ti = do
  let stmt = selectStmtFor ti id
  resultRowsSqlValues <- quickQuery conn stmt []
  let (resultRow :: [String]) = map convert (head resultRowsSqlValues)
  return $ expectJust ("No " ++ (show $ typeName ti) ++ " found for id " ++ id) (buildFromRecord ti resultRow :: Maybe a)
