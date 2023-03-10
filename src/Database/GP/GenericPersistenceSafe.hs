{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# OPTIONS_GHC -Wno-orphans     #-}
{-# LANGUAGE LambdaCase #-}

module Database.GP.GenericPersistenceSafe
  ( retrieveById,
    retrieveAll,
    retrieveAllWhere,
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
    PersistenceException(..)
  )
where

import           Data.Convertible         (ConvertResult, Convertible)
import           Data.Convertible.Base    (Convertible (safeConvert))
import           Data.List                (elemIndex, isInfixOf)
import           Database.GP.Conn
import           Database.GP.Entity
import           Database.GP.SqlGenerator
import           Database.GP.TypeInfo
import           Database.HDBC
import Control.Monad (when)
import Control.Exception (Exception, SomeException, try)

{- | 
 This is the "safe" version of the module Database.GP.GenericPersistence. It uses Either to return errors.

 This module defines RDBMS Persistence operations for Record Data Types that are instances of 'Data'.
 I call instances of such a data type Entities.

 The Persistence operations are using Haskell generics to provide compile time reflection capabilities.
 HDBC is used to access the RDBMS.
-}

-- | exceptions that may occur during persistence operations
data PersistenceException = 
    EntityNotFound String
  | DuplicateInsert String
  | DatabaseError String
  | NoUniqueKey String
  deriving (Show, Eq, Exception)

fromException :: SomeException -> PersistenceException
--fromException pe@(EntityNotFound msg) = pe
fromException ex = DatabaseError $ show ex

-- | A function that retrieves an entity from a database.
-- The function takes entity id as parameter.
-- If an entity with the given id exists in the database, it is returned as a Just value.
-- If no such entity exists, Nothing is returned.
-- An error is thrown if there are more than one entity with the given id.
retrieveById :: forall a id. (Entity a, Convertible id SqlValue) => Conn -> id -> IO (Either PersistenceException a)
retrieveById conn idx = do
  eitherExResultRows <- try $ quickQuery conn stmt [eid]
  case eitherExResultRows of
    Left ex -> return $ Left $ fromException ex
    Right resultRowsSqlValues -> 
      case resultRowsSqlValues of
        [] -> return $ Left $ EntityNotFound $ constructorName ti ++ " " ++ show eid ++ " not found"
        [singleRow] -> do
          eitherExEntity <- try $ fromRow conn singleRow
          case eitherExEntity of
            Left ex -> return $ Left $ fromException ex
            Right entity -> return $ Right entity
        _ -> return $ Left $ NoUniqueKey $ "More than one " ++ constructorName ti ++ " found for id " ++ show eid
  where
    ti = typeInfo @a
    stmt = selectStmtFor @a
    eid = toSql idx



-- | This function retrieves all entities of type `a` from a database.
--  The function takes an HDBC connection as parameter.
--  The type `a` is determined by the context of the function call.
retrieveAll :: forall a. (Entity a) => Conn -> IO (Either PersistenceException [a])
retrieveAll conn = do
  eitherExRows <- tryPE $ quickQuery conn stmt []
  case eitherExRows of
    Left ex -> return $ Left ex
    Right resultRows -> entitiesFromRows conn resultRows
  where
    stmt = selectAllStmtFor @a

-- | This function retrieves all entities of type `a` where a given field has a given value.
--  The function takes an HDBC connection, the name of the field and the value as parameters.
--  The type `a` is determined by the context of the function call.
--  The function returns a (possibly empty) list of all matching entities.
retrieveAllWhere :: forall a. (Entity a) => Conn -> String -> SqlValue -> IO (Either PersistenceException [a])
retrieveAllWhere conn field val = do
  eitherExRows <- tryPE $ quickQuery conn stmt [val]
  case eitherExRows of
    Left ex -> return $ Left ex
    Right resultRows -> entitiesFromRows conn resultRows
  where
    stmt = selectAllWhereStmtFor @a field

-- | This function converts a list of database rows, represented as a `[[SqlValue]]` to a list of entities.
--   The function takes an HDBC connection and a list of database rows as parameters.
--   The type `a` is determined by the context of the function call.
--   The function returns a (possibly empty) list of all matching entities.
--   The function is used internally by `retrieveAll` and `retrieveAllWhere`.
--   But it can also be used to convert the result of a custom SQL query to a list of entities.
entitiesFromRows :: forall a. (Entity a) => Conn -> [[SqlValue]] -> IO (Either PersistenceException [a])
entitiesFromRows = (tryPE .) . mapM . fromRow 

-- | A function that persists an entity to a database.
-- The function takes an HDBC connection and an entity as parameters.
-- The entity is either inserted or updated, depending on whether it already exists in the database.
-- The required SQL statements are generated dynamically using Haskell generics and reflection
persist :: forall a. (Entity a) => Conn -> a -> IO (Either PersistenceException ())
persist conn entity = do
  eitherExRes <- try $
    idValue conn entity >>= \eid ->
    quickQuery conn preparedSelectStmt [eid] >>= 
      \case 
        []           -> insert conn entity
        [_singleRow] -> update conn entity
        _            -> error $ "More than one entity found for id " ++ show eid
  case eitherExRes of
    Left ex -> return $ Left $ fromException ex
    Right res -> return res
  where
    preparedSelectStmt = selectStmtFor @a

-- | A function that explicitely inserts an entity into a database.
insert :: forall a. (Entity a) => Conn -> a -> IO (Either PersistenceException ())
insert conn entity = do
  eitherExUnit <- try $ do
    row <- toRow conn entity
    _rowcount <- run conn (insertStmtFor @a) row
    when (implicitCommit conn) $ commit conn
  case eitherExUnit of
    Left ex -> return $ Left $ handleDuplicateInsert ex
    Right _ -> return $ Right ()
  
handleDuplicateInsert :: SomeException -> PersistenceException
handleDuplicateInsert ex = if "UNIQUE constraint failed" `isInfixOf` show ex 
  then DuplicateInsert "Entity already exists in DB, use update instead" 
  else fromException ex
  
tryPE :: IO a -> IO (Either PersistenceException a)
tryPE action = do
  eitherExResult <- try action
  case eitherExResult of
    Left ex -> return $ Left $ fromException ex
    Right result -> return $ Right result  

-- | A function that inserts a list of entities into a database.
--   The function takes an HDBC connection and a list of entities as parameters.
--   The insert-statement is compiled only once and then executed for each entity.
insertMany :: forall a. (Entity a) => Conn -> [a] -> IO (Either PersistenceException ())
insertMany conn entities = do
  eitherExUnit <- try $ do
    rows <- mapM (toRow conn) entities
    stmt <- prepare conn (insertStmtFor @a)
    executeMany stmt rows
    when (implicitCommit conn) $ commit conn
  case eitherExUnit of
    Left ex -> return $ Left $ handleDuplicateInsert ex
    Right _ -> return $ Right ()


-- | A function that explicitely updates an entity in a database.
update :: forall a. (Entity a) => Conn -> a -> IO (Either PersistenceException ())
update conn entity = do
  eitherExUnit <- try $ do
    eid <- idValue conn entity
    row <- toRow conn entity
    rowcount <- run conn (updateStmtFor @a) (row ++ [eid])
    if (rowcount == 0) 
      then return (Left (EntityNotFound (constructorName (typeInfo @a) ++ " " ++ show eid ++ " does not exist")))
      else do
        when (implicitCommit conn) $ commit conn
        return $ Right ()
  case eitherExUnit of
    Left ex -> return $ Left $ fromException ex
    Right result -> return result

-- | A function that updates a list of entities in a database.
--   The function takes an HDBC connection and a list of entities as parameters.
--   The update-statement is compiled only once and then executed for each entity.
updateMany :: forall a. (Entity a) => Conn -> [a] -> IO (Either PersistenceException ())
updateMany conn entities = tryPE $ do
  eids <- mapM (idValue conn) entities
  rows <- mapM (toRow conn) entities
  stmt <- prepare conn (updateStmtFor @a)
  -- the update statement has one more parameter than the row: the id value for the where clause
  executeMany stmt (zipWith (\l x -> l ++ [x]) rows eids)
  when (implicitCommit conn) $ commit conn

-- | A function that deletes an entity from a database.
--   The function takes an HDBC connection and an entity as parameters.
delete :: forall a. (Entity a) => Conn -> a -> IO (Either PersistenceException ())
delete conn entity = do
  eitherExRes <- try $ do
    eid <- idValue conn entity
    rowCount <- run conn (deleteStmtFor @a) [eid]
    if (rowCount == 0) 
      then return (Left (EntityNotFound (constructorName (typeInfo @a) ++ " " ++ show eid ++ " does not exist")))
      else do
        when (implicitCommit conn) $ commit conn
        return $ Right ()
  case eitherExRes of
    Left ex -> return $ Left $ fromException ex
    Right result -> return result

-- | A function that deletes a list of entities from a database.
--   The function takes an HDBC connection and a list of entities as parameters.
--   The delete-statement is compiled only once and then executed for each entity.
deleteMany :: forall a. (Entity a) => Conn -> [a] -> IO (Either PersistenceException ())
deleteMany conn entities = tryPE $ do
  eids <- mapM (idValue conn) entities
  stmt <- prepare conn (deleteStmtFor @a)
  executeMany stmt (map (: []) eids)
  when (implicitCommit conn) $ commit conn

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

-- | These instances are needed to make the Convertible type class work with Enum types out of the box.
--   This is needed because the Convertible type class is used to convert SqlValues to Haskell types.
instance {-# OVERLAPS #-} forall a. (Enum a) => Convertible SqlValue a where
  safeConvert :: SqlValue -> ConvertResult a
  safeConvert = Right . toEnum . fromSql

instance {-# OVERLAPS #-} forall a. (Enum a) => Convertible a SqlValue where
  safeConvert :: a -> ConvertResult SqlValue
  safeConvert = Right . toSql . fromEnum
