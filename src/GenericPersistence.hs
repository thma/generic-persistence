{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}

module GenericPersistence
  ( retrieveById,
    retrieveAll,
    persist,
    delete,
    Entity (..),
  )
where

import           Data.Convertible
import           Database.HDBC        (IConnection, SqlValue, commit,
                                       quickQuery, run, toSql)
import           Entity
import           GHC.Data.Maybe       (expectJust)
import           RecordtypeReflection
import           SqlGenerator
import           TypeInfo

{--
 This module defines RDBMS Persistence operations for Record Data Types that are instances of 'Data'.
 I call instances of such a data type Entities.

 The Persistence operations are using Haskell generics to provide compile time reflection capabilities.
 HDBC is used to access the RDBMS.
--}

-- | A function that retrieves an entity from a database.
-- The function takes an HDBC connection and an entity id as parameters.
-- It returns the entity of type `a` with the given id.
-- An error is thrown if no such entity exists or if there are more than one entity with the given id.
retrieveById :: forall a conn id. (Entity a, IConnection conn, Convertible id SqlValue) => conn -> id -> IO a
retrieveById conn idx = do
  trace $ "Retrieve " ++ typeName ti ++ " with id " ++ show eid
  resultRowsSqlValues <- quickQuery conn stmt [eid]
  case resultRowsSqlValues of
    [] -> error $ "No " ++ show (typeConstructor ti) ++ " found for id " ++ show eid
    [singleRowSqlValues] -> do
      return $
        expectJust
          ("No " ++ show (typeConstructor ti) ++ " found for id " ++ show eid)
          (buildFromRecord ti singleRowSqlValues :: Maybe a)
    _ -> error $ "More than one entity found for id " ++ show eid
  where
    ti = typeInfoFromContext
    stmt = preparedSelectStmtFor ti
    eid = toSql idx

-- | This function retrieves all entities of type `a` from a database.
--  The function takes an HDBC connection as parameter.
--  The type `a` is determined by the context of the function call.
retrieveAll :: forall a conn. (Entity a, IConnection conn) => conn -> IO [a]
retrieveAll conn = do
  trace $ "Retrieve all " ++ typeName ti ++ "s"
  resultRowsSqlValues <- quickQuery conn stmt []
  return $ map (expectJust "No entity found") (map (buildFromRecord ti) resultRowsSqlValues :: [Maybe a])
  where
    ti = typeInfoFromContext
    
    stmt
     = selectAllStmtFor ti

-- | A function that persists an entity  to a database.
-- The function takes an HDBC connection and an entity as parameters.
-- The entity is either inserted or updated, depending on whether it already exists in the database.
-- The required SQL statements are generated dynamically using Haskell generics and reflection
persist :: (IConnection conn, Entity a) => conn -> a -> IO ()
persist conn entity = do
  resultRows <- quickQuery conn preparedSelectStmt [eid]
  case resultRows of
    []           -> insert conn entity
    [_singleRow] -> update conn entity
    _            -> error $ "More than one entity found for id " ++ show eid
  where
    ti = typeInfo entity
    eid = entityId entity
    preparedSelectStmt = preparedSelectStmtFor ti

insert :: (IConnection conn, Entity a) => conn -> a -> IO ()
insert conn entity = do
  trace $ "Inserting " ++ toString entity
  _rowcount <- run conn (preparedInsertStmtFor entity) (toRow entity)
  commit conn

update :: (IConnection conn, Entity a) => conn -> a -> IO ()
update conn entity = do
  trace $ "Updating " ++ toString entity
  _rowcount <- run conn (preparedUpdateStmtFor entity) (toRow entity ++ [entityId entity])
  commit conn

-- | A function that returns the primary key value of an entity as a String.
entityId :: forall a. (Entity a) => a -> SqlValue
entityId x = fieldValue x (idField x)

delete :: (IConnection conn, Entity a) => conn -> a -> IO ()
delete conn entity = do
  trace $ "Deleting " ++ typeName (typeInfo entity) ++ " with id " ++ show (entityId entity)
  _rowCount <- run conn (preparedDeleteStmtFor entity) [entityId entity]
  commit conn

-- | A function that traces a string to the console.
trace :: String -> IO ()
trace = putStrLn
