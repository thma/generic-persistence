{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DefaultSignatures #-}

module GenericPersistence
  ( retrieveById,
    retrieveAll,
    persist,
    delete,
    Entity,
    fromRow,
    toRow
  )
where

import           Data.Data            ( Data )
import           Database.HDBC        (IConnection, commit, quickQuery, run, SqlValue, toSql)
import           GHC.Data.Maybe       (expectJust)
import           RecordtypeReflection 
import           SqlGenerator         
import           TypeInfo             
import Data.Convertible


{--
 This module defines RDBMS Persistence operations for Record Data Types that are instances of 'Data'.
 I call instances of such a data type Entities.

 The Persistence operations are using Haskell generics to provide compile time reflection capabilities.
 HDBC is used to access the RDBMS.
--}


class (Data a) => Entity a where
  fromRow :: [SqlValue] -> a
  toRow :: a -> [SqlValue]

  default fromRow :: [SqlValue] -> a
  fromRow = gFromRow

  default toRow :: a -> [SqlValue]
  toRow = gToRow


-- | A function that retrieves an entity from a database.
-- The function takes an HDBC connection and an entity id as parameters.
-- It returns the entity of type `a` with the given id.
-- An error is thrown if no such entity exists or if there are more than one entity with the given id.
retrieveById :: forall a conn id. (Entity a, IConnection conn, Convertible id SqlValue) => conn -> id -> IO a
retrieveById conn idx = do
  let ti = typeInfoFromContext 
      stmt = preparedSelectStmtFor ti
      eid = toSql idx
  trace $ "Retrieve " ++ tiTypeName ti ++ " with id " ++ show eid
  resultRowsSqlValues <- quickQuery conn stmt [eid]
  case resultRowsSqlValues of
    [] -> error $ "No " ++ show (typeConstructor ti) ++ " found for id " ++ show eid
    [singleRowSqlValues] -> do
      return $ 
        expectJust 
          ("No " ++ show (typeConstructor ti) ++ " found for id " ++ show eid) 
          (buildFromRecord ti singleRowSqlValues :: Maybe a)
    _ -> error $ "More than one entity found for id " ++ show eid
  

-- | This function retrieves all entities of type `a` from a database.
--  The function takes an HDBC connection as parameter.
--  The type `a` is determined by the context of the function call.
retrieveAll :: forall a conn. (Entity a, IConnection conn) => conn -> IO [a]
retrieveAll conn = do
  let ti = typeInfoFromContext
      stmt = selectAllStmtFor ti
  trace $ "Retrieve all " ++ tiTypeName ti ++ "s"
  resultRowsSqlValues <- quickQuery conn stmt []
  return $ map (expectJust "No entity found") (map (buildFromRecord ti) resultRowsSqlValues :: [Maybe a])


-- | A function that persists an entity  to a database.
-- The function takes an HDBC connection and an entity as parameters.
-- The entity is either inserted or updated, depending on whether it already exists in the database.
-- The required SQL statements are generated dynamically using Haskell generics and reflection
persist :: (IConnection conn, Entity a) => conn -> a -> IO ()
persist conn entity = do
  resultRows <- quickQuery conn preparedSelectStmt [eid]
  case resultRows of
    [] -> do
      trace $ "Inserting " ++ gshow entity
      --runRaw conn insertStmt
      _rowcount <- run conn preparedInsertStmt (toRow entity)
      commit conn
    [_singleRow] -> do
      trace $ "Updating " ++ gshow entity
      let args = toRow entity ++ [eid]
      _rowcount <- run conn preparedUpdateStmt args
      commit conn
    _ -> error $ "More than one entity found for id " ++ show eid
  where
    ti = typeInfo entity
    eid = toSql $ entityId entity
    preparedSelectStmt = preparedSelectStmtFor ti
    preparedInsertStmt = preparedInsertStmtFor entity
    preparedUpdateStmt = preparedUpdateStmtFor entity
    
-- | A function that returns the primary key value of an entity as a String.    
entityId :: forall d. (Data d) => d -> String
entityId x = fieldValueAsString x (idColumn (typeInfo x))    

delete :: (IConnection conn, Entity a) => conn -> a -> IO ()
delete conn entity = do
  trace $ "Deleting " ++ typeName entity ++ " with id " ++ entityId entity
  _rowCount <- run conn (preparedDeleteStmtFor entity) [toSql $ entityId entity]
  commit conn

-- | A function that traces a string to the console.
trace :: String -> IO ()
trace = putStrLn