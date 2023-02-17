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
    Entity (..),
    columnNameFor,
    fieldTypeFor,
    maybeFieldTypeFor,
    toString,
    evidence,
    evidenceFrom,
    ResolutionCache,
    EntityId,
    entityId,
    getElseRetrieve,
    TypeInfo (..),
    typeInfoFromContext,
    typeInfo,
    Ctx (..),
    GP,
    extendCtxCache,
    runGP,
    liftIO,
    local,
    ask,
  )
where

import           Data.Convertible                 (ConvertResult, Convertible)
import           Data.Convertible.Base            (Convertible (safeConvert))
import           Data.Data
import           Data.Dynamic                     (fromDynamic, toDyn)
import           Database.GP.Entity
import           Database.GP.SqlGenerator
import           Database.GP.TypeInfo
import           Database.HDBC
import           RIO
import           Data.List                      (elemIndex)
import           GHC.Data.Maybe (expectJust)
--import           GHC.Generics

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
retrieveById :: forall a id. (Entity a, Convertible id SqlValue) => id -> GP (Maybe a)
retrieveById idx = do
  conn <- askConnection
  resultRowsSqlValues <- liftIO $ quickQuery conn stmt [eid]
  case resultRowsSqlValues of
    []          -> pure Nothing
    [singleRow] -> Just <$> fromRow singleRow
    _ -> error $ "More than one" ++ show (typeConstructor ti) ++ " found for id " ++ show eid
  where
    ti = typeInfoFromContext :: TypeInfo a
    stmt = selectStmtFor ti
    eid = toSql idx


-- | This function retrieves all entities of type `a` from a database.
--  The function takes an HDBC connection as parameter.
--  The type `a` is determined by the context of the function call.
retrieveAll :: forall a. (Entity a) => GP [a]
retrieveAll = do
  conn <- askConnection
  resultRows <- liftIO $ quickQuery conn stmt []
  mapM fromRow resultRows
  where
    ti = typeInfoFromContext :: TypeInfo a
    stmt = selectAllStmtFor ti

retrieveAllWhere :: forall a. (Entity a) => String -> SqlValue -> GP [a]
retrieveAllWhere field val = do
  conn <- askConnection
  resultRows <- liftIO $ quickQuery conn stmt [val]
  mapM fromRow resultRows
  where
    ti = typeInfoFromContext :: TypeInfo a
    stmt = selectAllWhereStmtFor ti field

-- | A function that persists an entity to a database.
-- The function takes an HDBC connection and an entity as parameters.
-- The entity is either inserted or updated, depending on whether it already exists in the database.
-- The required SQL statements are generated dynamically using Haskell generics and reflection
persist :: (Entity a) => a -> GP ()
persist entity = do
  conn <- askConnection
  resultRows <- liftIO $ quickQuery conn preparedSelectStmt [eid]
  case resultRows of
    []           -> insert entity
    [_singleRow] -> update entity
    _            -> error $ "More than one entity found for id " ++ show eid
  where
    ti = typeInfo entity
    eid = idValue entity
    preparedSelectStmt = selectStmtFor ti

-- | A function that explicitely inserts an entity into a database.
insert :: (Entity a) => a -> GP ()
insert entity = do
  conn <- askConnection
  row <- toRow entity
  _rowcount <- liftIO $ run conn (insertStmtFor entity) row
  liftIO $ commit conn

-- | A function that explicitely updates an entity in a database.
update :: (Entity a) => a -> GP ()
update entity = do
  conn <- askConnection
  row <- toRow entity
  _rowcount <- liftIO $ run conn (updateStmtFor entity) (row ++ [idValue entity])
  liftIO $ commit conn

delete :: (Entity a) => a -> GP ()
delete entity = do
  conn <- askConnection
  _rowCount <- liftIO $ run conn (deleteStmtFor entity) [idValue entity]
  liftIO $ commit conn

-- | set up a table for a given entity type. The table is dropped and recreated.
setupTableFor :: forall a. (Entity a) => GP a
setupTableFor = do
  conn <- askConnection
  _ <- liftIO $ runRaw conn (dropTableStmtFor ti)
  _ <- liftIO $ runRaw conn (createTableStmtFor ti)
  liftIO $ commit conn
  return x
  where
    ti = typeInfoFromContext :: TypeInfo a
    x = evidenceFrom ti :: a


-- | Lookup an entity in the cache, or retrieve it from the database.
--   The Entity is identified by its EntityId, which is a (typeRep, idValue) tuple.
getElseRetrieve :: forall a . (Entity a) => EntityId -> GP (Maybe a)
getElseRetrieve eid@(_tr,pk) = do
  rc <- askCache
  case lookup eid rc of
    Just dyn -> case fromDynamic dyn :: Maybe a of
      Just e  -> pure (Just e)
      Nothing -> error "should not be possible"
    Nothing -> retrieveById pk :: GP (Maybe a)


extendCtxCache :: Entity a => a -> Ctx -> Ctx
extendCtxCache x (Ctx conn rc) = Ctx conn (cacheEntry : rc)
  where
    cacheEntry = (entityId x, toDyn x)


-- | Computes the EntityId of an entity.
--   The EntityId of an entity is a (typeRep, idValue) tuple.
entityId :: (Entity a) => a -> EntityId
entityId x = (typeOf x, idValue x)

-- | A function that returns the primary key value of an entity as a SqlValue.
idValue :: forall a. (Entity a) => a -> SqlValue
idValue x = sqlValues !! idFieldIndex 
  where
    idFieldIndex = fieldIndex x (idField x)
    sqlValues = toRowWoCtx x

-- | returns the index of a field of an entity.
--   The index is the position of the field in the list of fields of the entity.
--   If no such field exists, an error is thrown.
fieldIndex :: (Entity a) => a -> String -> Int
fieldIndex x fieldName =
  expectJust
    ("Field " ++ fieldName ++ " is not present in type " ++ typeName ti)
    (elemIndex fieldName fieldList) 
  where
    ti = typeInfo x
    fieldList = fieldNames ti

askConnection :: GP ConnWrapper
askConnection = connection <$> ask

askCache :: GP ResolutionCache
askCache = cache <$> ask

runGP :: (MonadIO m, IConnection conn) => conn -> RIO Ctx a -> m a
runGP conn = runRIO (Ctx (ConnWrapper conn) mempty)

-- These instances are needed to make the Convertible type class work with Enum types out of the box.
instance {-# OVERLAPS #-} forall a . (Enum a) => Convertible SqlValue a where
  safeConvert :: SqlValue -> ConvertResult a
  safeConvert = Right . toEnum . fromSql

instance {-# OVERLAPS #-} forall a . (Enum a) => Convertible a SqlValue where
  safeConvert :: a -> ConvertResult SqlValue
  safeConvert = Right . toSql . fromEnum
