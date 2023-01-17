{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module GenericPersistence
  ( retrieveEntityById,
    retrieveAllEntities,
    persistEntity,
    deleteEntity,
  )
where

import           Data.Data
import           Database.HDBC        (IConnection, commit, quickQuery, runRaw)
import           GHC.Data.Maybe       (expectJust)
import           RecordtypeReflection (buildFromRecord, fieldValueAsString)
import           SqlGenerator         (deleteStmtFor, idColumn, insertStmtFor,
                                       selectAllStmtFor, selectStmtFor,
                                       updateStmtFor)
import           TypeInfo             (TypeInfo, gshow, typeInfo, typeName)

{--
 This module defines RDBMS Persistence operations for Record Data Types that are instances of 'Data'.
 I call instances of such a data type Entities.

 The Persistence operations are using Haskell generics to provide compile time reflection capabilities.
 HDBC is used to access the RDBMS.
--}

-- | A function that retrieves an entity from a database.
-- I would like to get rid of the TypeInfo paraemeter and derive it directly from the 'IO a' result type.
-- This will need some helping hand from the Internet...
retrieveEntityById :: forall a conn id. (Data a, IConnection conn, Show id) => conn -> TypeInfo -> id -> IO a
retrieveEntityById conn ti eid = do
  let stmt = selectStmtFor ti eid
  resultRowsSqlValues <- quickQuery conn stmt []
  case resultRowsSqlValues of
    [] -> error $ "No " ++ show (typeName ti) ++ " found for id " ++ show eid
    [singleRowSqlValues] -> do
      return $ expectJust ("No " ++ show (typeName ti) ++ " found for id " ++ show eid) (buildFromRecord ti singleRowSqlValues :: Maybe a)
    _ -> error $ "More than one entity found for id " ++ show eid

retrieveAllEntities :: forall a conn. (Data a, IConnection conn) => conn -> TypeInfo -> IO [a]
retrieveAllEntities conn ti = do
  let stmt = selectAllStmtFor ti
  resultRowsSqlValues <- quickQuery conn stmt []
  return $ map (expectJust "No entity found") (map (buildFromRecord ti) resultRowsSqlValues :: [Maybe a])

-- | A function that persists an entity  to a database.
-- The function takes an HDBC connection and an entity (fulfilling constraint 'Data a') as parameters.
-- The entity is either inserted or updated, depending on whether it already exists in the database.
-- The required SQL statements are generated dynamically using Haskell generics and reflection
persistEntity :: (IConnection conn, Data a) => conn -> a -> IO ()
persistEntity conn entity = do
  resultRows <- quickQuery conn selectStmt []
  case resultRows of
    [] -> do
      putStrLn $ "Inserting " ++ gshow entity
      runRaw conn insertStmt
      commit conn
    [_singleRow] -> do
      putStrLn $ "Updating " ++ gshow entity
      runRaw conn updateStmt
      commit conn
    _ -> error $ "More than one entity found for id " ++ show eid
  where
    ti = typeInfo entity
    eid = entityId entity
    selectStmt = selectStmtFor ti eid
    insertStmt = insertStmtFor entity
    updateStmt = updateStmtFor entity
    
    entityId :: forall d. (Data d) => d -> String
    entityId x = fieldValueAsString x (idColumn (typeInfo x))
    

--deleteEntity :: forall a conn. (Data a, IConnection conn) => conn -> a -> IO ()
deleteEntity :: (IConnection conn, Data a) => conn -> a -> IO ()
deleteEntity conn entity = do
  runRaw conn (deleteStmtFor entity)
  commit conn
