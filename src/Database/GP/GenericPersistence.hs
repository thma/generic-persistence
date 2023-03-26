{-# LANGUAGE AllowAmbiguousTypes #-}
module Database.GP.GenericPersistence
  ( retrieveById,
    retrieveAll,
    retrieveWhere,
    entitiesFromRows,
    persist,
    insert,
    insertMany,
    update,
    updateMany,
    delete,
    deleteMany,
    setupTableFor,
    idValue,
    Conn(..),
    connect,
    Database(..),
    Entity (..),
    GToRow,
    GFromRow,
    columnNameFor,
    maybeFieldTypeFor,
    toString,
    TypeInfo (..),
    typeInfo,
    PersistenceException(..),
    WhereClauseExpr,
    (&&.),
    (||.),
    (!.),
    (==.),
    (>.),
    (<.),
    (>=.),
    (<=.),
    (!=.),
    like,
  )
where

import           Control.Exception
import           Control.Monad                      (when)
import           Data.Convertible                   (Convertible)
import           Data.List                          (elemIndex)
import           Database.GP.Conn
import           Database.GP.Entity
import           Database.GP.GenericPersistenceSafe (PersistenceException)
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
retrieveById :: forall a id. (Entity a, Convertible id SqlValue) => Conn -> id -> IO (Maybe a)
retrieveById conn idx = do
  eitherExEntity <- GpSafe.retrieveById conn idx
  case eitherExEntity of
    Left (GpSafe.EntityNotFound _) -> pure Nothing
    Left ex                        -> throw ex
    Right entity                   -> pure $ Just entity

-- | This function retrieves all entities of type `a` from a database.
--  The function takes an HDBC connection as parameter.
--  The type `a` is determined by the context of the function call.
retrieveAll :: forall a. (Entity a) => Conn -> IO [a]
retrieveAll conn = do
  eitherExRow <- GpSafe.retrieveAll @a conn
  case eitherExRow of
    Left ex    -> throw ex
    Right rows -> pure rows

-- | This function retrieves all entities of type `a` that match some query criteria.
--   The function takes an HDBC connection and a `WhereClauseExpr` as parameters.
--   The type `a` is determined by the context of the function call.
--   The function returns a (possibly empty) list of all matching entities.
--   The `WhereClauseExpr` is typically constructed using any tiny query dsl based on infix operators.
retrieveWhere :: forall a. (Entity a) => Conn -> WhereClauseExpr -> IO [a]
retrieveWhere conn whereClause = do
  eitherExEntities <- GpSafe.retrieveWhere @a conn whereClause
  case eitherExEntities of
    Left ex        -> throw ex
    Right entities -> pure entities

-- | This function converts a list of database rows, represented as a `[[SqlValue]]` to a list of entities.
--   The function takes an HDBC connection and a list of database rows as parameters.
--   The type `a` is determined by the context of the function call.
--   The function returns a (possibly empty) list of all matching entities.
--   The function is used internally by `retrieveAll` and `retrieveAllWhere`.
--   But it can also be used to convert the result of a custom SQL query to a list of entities.
entitiesFromRows :: forall a. (Entity a) => Conn -> [[SqlValue]] -> IO [a]
entitiesFromRows conn rows = do
  eitherExEntities <- GpSafe.entitiesFromRows @a conn rows
  case eitherExEntities of
    Left ex        -> throw ex
    Right entities -> pure entities

fromEitherExUnit :: IO (Either PersistenceException ()) -> IO ()
fromEitherExUnit ioEitherExUnit = do
  eitherExUnit <- ioEitherExUnit
  case eitherExUnit of
    Left ex -> throw ex
    Right _ -> pure ()

-- | A function that persists an entity to a database.
-- The function takes an HDBC connection and an entity as parameters.
-- The entity is either inserted or updated, depending on whether it already exists in the database.
-- The required SQL statements are generated dynamically using Haskell generics and reflection
persist :: forall a. (Entity a) => Conn -> a -> IO ()
persist = (fromEitherExUnit .) . GpSafe.persist

-- | A function that explicitely inserts an entity into a database.
insert :: forall a. (Entity a) => Conn -> a -> IO ()
insert = (fromEitherExUnit .) . GpSafe.insert

-- | A function that inserts a list of entities into a database.
--   The function takes an HDBC connection and a list of entities as parameters.
--   The insert-statement is compiled only once and then executed for each entity.
insertMany :: forall a. (Entity a) => Conn -> [a] -> IO ()
insertMany = (fromEitherExUnit .) . GpSafe.insertMany

-- | A function that explicitely updates an entity in a database.
update :: forall a. (Entity a) => Conn -> a -> IO ()
update = (fromEitherExUnit .) . GpSafe.update

-- | A function that updates a list of entities in a database.
--   The function takes an HDBC connection and a list of entities as parameters.
--   The update-statement is compiled only once and then executed for each entity.
updateMany :: forall a. (Entity a) => Conn -> [a] -> IO ()
updateMany = (fromEitherExUnit .) . GpSafe.updateMany

-- | A function that deletes an entity from a database.
--   The function takes an HDBC connection and an entity as parameters.
delete :: forall a. (Entity a) => Conn -> a -> IO ()
delete = (fromEitherExUnit .) . GpSafe.delete

-- | A function that deletes a list of entities from a database.
--   The function takes an HDBC connection and a list of entities as parameters.
--   The delete-statement is compiled only once and then executed for each entity.
deleteMany :: forall a. (Entity a) => Conn -> [a] -> IO ()
deleteMany = (fromEitherExUnit .) . GpSafe.deleteMany

-- | set up a table for a given entity type. The table is dropped (if existing) and recreated.
--   The function takes an HDBC connection as parameter.
setupTableFor :: forall a. (Entity a) => Conn -> IO ()
setupTableFor conn = do
  runRaw conn $ dropTableStmtFor @a
  runRaw conn $ createTableStmtFor @a (db conn)
  when (implicitCommit conn) $ commit conn

-- | A function that returns the primary key value of an entity as a SqlValue.
--   The function takes an HDBC connection and an entity as parameters.
idValue :: forall a. (Entity a) => Conn -> a -> IO SqlValue
idValue conn x = do
  sqlValues <- toRow conn x
  return (sqlValues !! idFieldIndex)
  where
    idFieldIndex = fieldIndex @a (idField @a)

-- | returns the index of a field of an entity.
--   The index is the position of the field in the list of fields of the entity.
--   If no such field exists, an error is thrown.
--   The function takes an field name as parameters,
--   the type of the entity is determined by the context.
fieldIndex :: forall a. (Entity a) => String -> Int
fieldIndex fieldName =
  expectJust
    ("Field " ++ fieldName ++ " is not present in type " ++ constructorName ti)
    (elemIndex fieldName fieldList)
  where
    ti = typeInfo @a
    fieldList = fieldNames ti

expectJust :: String -> Maybe a -> a
expectJust _ (Just x)  = x
expectJust err Nothing = error ("expectJust " ++ err)
