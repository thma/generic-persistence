module GenericPersistence
  ( retrieveById,
    retrieveAll,
    retrieveAllWhere,
    persist,
    insert,
    update,
    delete,
    setupTableFor,
    idValue,
    Entity (..),
  )
where

import Data.Convertible ( Convertible )
import           Database.HDBC        (IConnection, SqlValue, commit,
                                       quickQuery, run, toSql, runRaw)
import           Entity
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
  resultRowsSqlValues <- quickQuery conn stmt [eid]
  case resultRowsSqlValues of
    []          -> error $ "No " ++ show (typeConstructor ti) ++ " found for id " ++ show eid
    [singleRow] -> fromRow conn singleRow
    _ -> error $ "More than one" ++ show (typeConstructor ti) ++ " found for id " ++ show eid
  where
    ti = typeInfoFromContext :: TypeInfo a
    stmt = selectStmtFor ti
    eid = toSql idx

-- | This function retrieves all entities of type `a` from a database.
--  The function takes an HDBC connection as parameter.
--  The type `a` is determined by the context of the function call.
retrieveAll :: forall a conn. (Entity a, IConnection conn) => conn -> IO [a]
retrieveAll conn = do
  resultRows <- quickQuery conn stmt []
  mapM (fromRow conn) resultRows
  where
    ti = typeInfoFromContext :: TypeInfo a
    stmt = selectAllStmtFor ti 

retrieveAllWhere :: forall a conn. (Entity a, IConnection conn) => conn -> String -> SqlValue -> IO [a]
retrieveAllWhere conn field val = do
  resultRows <- quickQuery conn stmt [val]
  mapM (fromRow conn) resultRows
  where
    ti = typeInfoFromContext :: TypeInfo a
    stmt = selectAllWhereStmtFor ti field

-- | A function that persists an entity to a database.
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
    eid = idValue entity
    preparedSelectStmt = selectStmtFor ti

-- | A function that explicitely inserts an entity into a database.
insert :: (IConnection conn, Entity a) => conn -> a -> IO ()
insert conn entity = do
  row <- toRow conn entity
  _rowcount <- run conn (insertStmtFor entity) row
  commit conn

-- | A function that explicitely updates an entity in a database.
update :: (IConnection conn, Entity a) => conn -> a -> IO ()
update conn entity = do
  row <- toRow conn entity
  _rowcount <- run conn (updateStmtFor entity) (row ++ [idValue entity])
  commit conn

-- | A function that returns the primary key value of an entity as a SqlValue.
idValue :: forall a. (Entity a) => a -> SqlValue
idValue x = fieldValue x (idField x)

delete :: (IConnection conn, Entity a) => conn -> a -> IO ()
delete conn entity = do
  _rowCount <- run conn (deleteStmtFor entity) [idValue entity]
  commit conn

-- | set up a table for a given entity type. The table is dropped and recreated.
setupTableFor :: forall a conn. (Entity a, IConnection conn) => conn -> IO a
setupTableFor conn = do
  _ <- runRaw conn (dropTableStmtFor ti)
  _ <- runRaw conn (createTableStmtFor ti)
  commit conn
  return x
  where
    ti = typeInfoFromContext :: TypeInfo a
    x = evidenceFrom ti :: a