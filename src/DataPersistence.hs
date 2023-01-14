{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE QuantifiedConstraints#-}
module DataPersistence
  (
    retrieveEntityById
  , persistEntity
  , deleteEntity
  )
where

import Database.HDBC (IConnection, quickQuery, run, commit)
import Data.Convertible.Base (convert, safeConvert)
import TypeInfo
import Data.Data
import RecordtypeReflection
import SqlGenerator
import GHC.Data.Maybe (expectJust)
import Data.Maybe (fromMaybe)


{--
 This module defines RDBMS Persistence operations for Record Data Types that are instances of 'Data'.
 I call instances of such a data type Entities.
   
 The Persistence operations are using Haskell generics to provide compile time reflection capabilities.
 HDBC is used to access the RDBMS.
--}


-- | A function that retrieves an entity from a database.
-- I would like to get rid of the TypeInfo paraemeter and derive it directly from the 'IO a' result type.
-- This will need some helping hand from the Internet...
retrieveEntityById :: forall a conn id . (Data a, IConnection conn, Show id) => conn -> TypeInfo -> id -> IO a
retrieveEntityById conn ti id = do
  let stmt = selectStmtFor ti id
  resultRowsSqlValues <- quickQuery conn stmt []
  case resultRowsSqlValues of
    [] -> error $ "No " ++ show (typeName ti) ++ " found for id " ++ show id
    [singleRowSqlValues] -> do
      let (resultRow :: [String]) = map convert singleRowSqlValues
      return $ expectJust ("No " ++ (show $ typeName ti) ++ " found for id " ++ show id) (buildFromRecord ti resultRow :: Maybe a)
    _ -> error $ "More than one entity found for id " ++ show id

 
persistEntity :: forall a conn . (Data a, Show a, IConnection conn) => conn -> a -> IO ()
persistEntity conn entity = do
  let ti = typeInfo entity
      id = entityId entity
      selectStmt = selectStmtFor ti id
      insertStmt = insertStmtFor entity
      updateStmt = updateStmtFor entity
  resultRows <- quickQuery conn selectStmt []
  case resultRows of
    [] -> do
      putStrLn $ "Inserting " ++ show entity
      run conn insertStmt []
      commit conn
    [singleRow] -> do
      putStrLn $ "Updating " ++ show entity
      run conn updateStmt []
      commit conn
    _ -> error $ "More than one entity found for id " ++ show id
  

entityId :: forall a . (Data a) => a -> String
entityId entity = fieldValueAsString entity idField
  where
    idField = idColumn (typeInfo entity)

deleteEntity :: forall a conn . (Data a, Show a, IConnection conn) => conn -> a -> IO ()
deleteEntity conn entity = do
  let ti = typeInfo entity
      id = entityId entity
      deleteStmt = deleteStmtFor entity
  run conn deleteStmt []
  commit conn
  
