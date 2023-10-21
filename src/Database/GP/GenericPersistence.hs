{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.GP.GenericPersistence
  ( selectById,
    select,
    entitiesFromRows,
    sql,
    persist,
    insert,
    insertMany,
    update,
    updateMany,
    delete,
    deleteMany,
    setupTableFor,
    setupTable,
    defaultSqliteMapping,
    defaultPostgresMapping,
    Conn (..),
    connect,
    Database (..),
    TxHandling (..),
    ConnectionPool,
    createConnPool,
    withResource,
    Entity (..),
    GToRow,
    GFromRow,
    columnNameFor,
    maybeFieldTypeFor,
    TypeInfo (..),
    typeInfo,
    PersistenceException (..),
    WhereClauseExpr,
    Field,
    field,
    (&&.),
    (||.),
    (=.),
    (>.),
    (<.),
    (>=.),
    (<=.),
    (<>.),
    like,
    -- contains,
    between,
    in',
    isNull,
    not',
    sqlFun,
    allEntries,
    byId,
    orderBy,
    SortOrder (..),
    limit,
    limitOffset,
    NonEmpty (..),
  )
where

import           Control.Exception
import           Data.Convertible                   (Convertible)
import           Database.GP.Conn
import           Database.GP.Entity
import           Database.GP.GenericPersistenceSafe (PersistenceException,
                                                     setupTableFor, setupTable, sql)
import qualified Database.GP.GenericPersistenceSafe as GpSafe
import           Database.GP.SqlGenerator
import           Database.GP.TypeInfo
import           Database.HDBC

-- |
-- This module defines RDBMS Persistence operations for Record Data Types that are instances of 'Data'.
-- I call instances of such a data type Entities.
--
-- The Persistence operations are using Haskell generics to provide compile time reflection capabilities.
-- HDBC is used to access the RDBMS.

-- | A function that retrieves an entity from a database.
-- The function takes entity id as parameter.
-- If an entity with the given id exists in the database, it is returned as a Just value.
-- If no such entity exists, Nothing is returned.
-- An error is thrown if there are more than one entity with the given id.
selectById :: forall a id. (Entity a, Convertible id SqlValue) => Conn -> id -> IO (Maybe a)
selectById conn idx = do
  eitherExEntity <- GpSafe.selectById conn idx
  case eitherExEntity of
    Left (GpSafe.EntityNotFound _) -> pure Nothing
    Left ex                        -> throw ex
    Right entity                   -> pure $ Just entity

-- | This function retrieves all entities of type `a` from a database.
--  The function takes an HDBC connection as parameter.
--  The type `a` is determined by the context of the function call.
-- retrieveAll :: forall a. (Entity a) => Conn -> IO [a]
-- retrieveAll conn = do
--   eitherExRow <- GpSafe.retrieveAll @a conn
--   case eitherExRow of
--     Left ex    -> throw ex
--     Right rows -> pure rows

-- | This function retrieves all entities of type `a` that match some query criteria.
--   The function takes an HDBC connection and a `WhereClauseExpr` as parameters.
--   The type `a` is determined by the context of the function call.
--   The function returns a (possibly empty) list of all matching entities.
--   The `WhereClauseExpr` is typically constructed using any tiny query dsl based on infix operators.
select :: forall a. (Entity a) => Conn -> WhereClauseExpr -> IO [a]
select conn whereClause = do
  eitherExEntities <- GpSafe.select @a conn whereClause
  case eitherExEntities of
    Left ex        -> throw ex
    Right entities -> pure entities

fromEitherExOrA :: IO (Either PersistenceException a) -> IO a
fromEitherExOrA ioEitherExUnit = do
  eitherExUnit <- ioEitherExUnit
  case eitherExUnit of
    Left ex -> throw ex
    Right a -> pure a

-- | A function that constructs a list of entities from a list of rows.
--   The function takes an HDBC connection and a list of rows as parameters.
--   The type `a` is determined by the context of the function call.
--   The function returns a list of entities.
--   This can be useful if you want to use your own SQL queries.
entitiesFromRows :: forall a. (Entity a) => Conn -> [[SqlValue]] -> IO [a]
entitiesFromRows = (fromEitherExOrA .) . GpSafe.entitiesFromRows

-- | A function that persists an entity to a database.
-- The function takes an HDBC connection and an entity as parameters.
-- The entity is either inserted or updated, depending on whether it already exists in the database.
-- The required SQL statements are generated dynamically using Haskell generics and reflection
persist :: forall a. (Entity a) => Conn -> a -> IO ()
persist = (fromEitherExOrA .) . GpSafe.persist

-- | A function that explicitely inserts an entity into a database.
insert :: forall a. (Entity a) => Conn -> a -> IO a
insert = (fromEitherExOrA .) . GpSafe.insert

-- | A function that inserts a list of entities into a database.
--   The function takes an HDBC connection and a list of entities as parameters.
--   The insert-statement is compiled only once and then executed for each entity.
insertMany :: forall a. (Entity a) => Conn -> [a] -> IO ()
insertMany = (fromEitherExOrA .) . GpSafe.insertMany

-- | A function that explicitely updates an entity in a database.
update :: forall a. (Entity a) => Conn -> a -> IO ()
update = (fromEitherExOrA .) . GpSafe.update

-- | A function that updates a list of entities in a database.
--   The function takes an HDBC connection and a list of entities as parameters.
--   The update-statement is compiled only once and then executed for each entity.
updateMany :: forall a. (Entity a) => Conn -> [a] -> IO ()
updateMany = (fromEitherExOrA .) . GpSafe.updateMany

-- | A function that deletes an entity from a database.
--   The function takes an HDBC connection and an entity as parameters.
delete :: forall a. (Entity a) => Conn -> a -> IO ()
delete = (fromEitherExOrA .) . GpSafe.delete

-- | A function that deletes a list of entities from a database.
--   The function takes an HDBC connection and a list of entities as parameters.
--   The delete-statement is compiled only once and then executed for each entity.
deleteMany :: forall a. (Entity a) => Conn -> [a] -> IO ()
deleteMany = (fromEitherExOrA .) . GpSafe.deleteMany


