{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Database.GP.GenericPersistence
  ( retrieveById,
    retrieveAll,
    retrieveAllWhere,
    persist,
    insert,
    update,
    delete,
    setupTableFor,
    idValue,
    Conn,
    Entity (..),
    GToRow,
    GFromRow,
    columnNameFor,
    maybeFieldTypeFor,
    toString,
    EntityId,
    entityId,
    TypeInfo (..),
    typeInfo,
  )
where

import           Data.Convertible         (ConvertResult, Convertible)
import           Data.Convertible.Base    (Convertible (safeConvert))
import           Data.List                (elemIndex)
import           Database.GP.Entity
import           Database.GP.SqlGenerator
import           Database.GP.TypeInfo
import           Database.HDBC

{--
 This module defines RDBMS Persistence operations for Record Data Types that are instances of 'Data'.
 I call instances of such a data type Entities.

 The Persistence operations are using Haskell generics to provide compile time reflection capabilities.
 HDBC is used to access the RDBMS.
--}

-- | A function that retrieves an entity from a database.
-- The function takes entity id as parameter.
-- If an entity with the given id exists in the database, it is returned as a Just value.
-- If no such entity exists, Nothing is returned.
-- An error is thrown if there are more than one entity with the given id.
retrieveById :: forall a id. (Entity a, Convertible id SqlValue) => Conn -> id -> IO (Maybe a)
retrieveById conn idx = do
  resultRowsSqlValues <- quickQuery conn stmt [eid]
  case resultRowsSqlValues of
    [] -> pure Nothing
    [singleRow] -> Just <$> fromRow conn singleRow
    _ -> error $ "More than one" ++ constructorName ti ++ " found for id " ++ show eid
  where
    ti = typeInfo @a
    stmt = selectStmtFor @a
    eid = toSql idx

-- | This function retrieves all entities of type `a` from a database.
--  The function takes an HDBC connection as parameter.
--  The type `a` is determined by the context of the function call.
retrieveAll :: forall a. (Entity a) => Conn -> IO [a]
retrieveAll conn = do
  resultRows <- quickQuery conn stmt []
  mapM (fromRow conn) resultRows
  where
    stmt = selectAllStmtFor @a

retrieveAllWhere :: forall a. (Entity a) => Conn -> String -> SqlValue -> IO [a]
retrieveAllWhere conn field val = do
  resultRows <- quickQuery conn stmt [val]
  mapM (fromRow conn) resultRows
  where
    stmt = selectAllWhereStmtFor @a field

-- | A function that persists an entity to a database.
-- The function takes an HDBC connection and an entity as parameters.
-- The entity is either inserted or updated, depending on whether it already exists in the database.
-- The required SQL statements are generated dynamically using Haskell generics and reflection
persist :: forall a. (Entity a) => Conn -> a -> IO ()
persist conn entity = do
  eid <- idValue conn entity
  resultRows <- quickQuery conn preparedSelectStmt [eid]
  case resultRows of
    []           -> insert conn entity
    [_singleRow] -> update conn entity
    _            -> error $ "More than one entity found for id " ++ show eid
  where
    preparedSelectStmt = selectStmtFor @a

-- | A function that explicitely inserts an entity into a database.
insert :: forall a. (Entity a) => Conn -> a -> IO ()
insert conn entity = do
  row <- toRow conn entity
  _rowcount <- run conn (insertStmtFor @a) row
  commit conn

-- | A function that explicitely updates an entity in a database.
update :: forall a. (Entity a) => Conn -> a -> IO ()
update conn entity = do
  eid <- idValue conn entity
  row <- toRow conn entity
  _rowcount <- run conn (updateStmtFor @a) (row ++ [eid])
  commit conn

delete :: forall a. (Entity a) => Conn -> a -> IO ()
delete conn entity = do
  eid <- idValue conn entity
  _rowCount <- run conn (deleteStmtFor @a) [eid]
  commit conn

-- | set up a table for a given entity type. The table is dropped and recreated.
setupTableFor :: forall a. (Entity a) => Conn -> IO a
setupTableFor conn = do
  _ <- runRaw conn $ dropTableStmtFor @a
  _ <- runRaw conn $ createTableStmtFor @a
  commit conn
  return x
  where
    x = undefined :: a

-- | Computes the EntityId of an entity.
--   The EntityId of an entity is a (typeRep, idValue) tuple.
entityId :: forall a. (Entity a) => Conn -> a -> IO EntityId
entityId conn x = do
  eid <- idValue conn x
  return (tyName, eid)
  where
    tyName = constructorName (typeInfo @a)

-- | A function that returns the primary key value of an entity as a SqlValue.
idValue :: forall a. (Entity a) => Conn -> a -> IO SqlValue
idValue conn x = do
  sqlValues <- toRow conn x
  return (sqlValues !! idFieldIndex)
  where
    idFieldIndex = fieldIndex @a (idField @a)

-- | returns the index of a field of an entity.
--   The index is the position of the field in the list of fields of the entity.
--   If no such field exists, an error is thrown.
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

-- These instances are needed to make the Convertible type class work with Enum types out of the box.
instance {-# OVERLAPS #-} forall a. (Enum a) => Convertible SqlValue a where
  safeConvert :: SqlValue -> ConvertResult a
  safeConvert = Right . toEnum . fromSql

instance {-# OVERLAPS #-} forall a. (Enum a) => Convertible a SqlValue where
  safeConvert :: a -> ConvertResult SqlValue
  safeConvert = Right . toSql . fromEnum
