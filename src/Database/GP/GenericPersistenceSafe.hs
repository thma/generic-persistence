{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Database.GP.GenericPersistenceSafe
  ( selectById,
    select,
    count,
    entitiesFromRows,
    sql,
    persist,
    upsert,
    insert,
    insertMany,
    update,
    updateMany,
    delete,
    deleteById,
    deleteMany,
    deleteManyById,
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
    fieldIndex,
    handleDuplicateInsert,
    removeAutoIncIdField,
  )
where

import           Control.Exception         (Exception, SomeException, try)
import           Control.Monad             (when)
import           Data.Convertible          (ConvertResult, Convertible)
import           Data.Convertible.Base     (Convertible (safeConvert))
import           Data.List                 (isInfixOf)
import           Database.GP.Conn
import           Database.GP.Entity
import           Database.GP.SqlGenerator
import           Database.GP.TypeInfo
import           Database.HDBC
import           Language.Haskell.TH.Quote (QuasiQuoter)
import           Text.RawString.QQ         (r)

-- |
-- This is the "safe" version of the module Database.GP.GenericPersistence. It uses Either to return errors.
--
-- This module defines RDBMS Persistence operations for Record Data Types that are instances of 'Data'.
-- I call instances of such a data type Entities.
--
-- The Persistence operations are using Haskell generics to provide compile time reflection capabilities.
-- HDBC is used to access the RDBMS.

-- | exceptions that may occur during persistence operations
data PersistenceException
  = EntityNotFound String
  | DuplicateInsert String
  | DatabaseError String
  | NoUniqueKey String
  deriving (Show, Eq, Exception)

-- | A function that retrieves an entity from a database.
-- The function takes entity id as parameter.
-- If an entity with the given id exists in the database, it is returned as a Just value.
-- If no such entity exists, Nothing is returned.
-- An error is thrown if there are more than one entity with the given id.
selectById :: forall a id. (Entity a, Convertible id SqlValue) => Conn -> id -> IO (Either PersistenceException a)
selectById conn idx = do
  -- print stmt
  eitherExResultRows <- try $ quickQuery conn stmt [eid]
  case eitherExResultRows of
    Left ex -> return $ Left $ fromException ex
    Right resultRowsSqlValues ->
      case resultRowsSqlValues of
        [] -> return $ Left $ EntityNotFound $ constructorName ti ++ " " ++ show eid ++ " not found"
        [singleRow] -> do
          eitherExEntity <- try $ fromRow conn singleRow
          case eitherExEntity of
            Left ex      -> return $ Left $ fromException ex
            Right entity -> return $ Right entity
        _ -> return $ Left $ NoUniqueKey $ "More than one " ++ constructorName ti ++ " found for id " ++ show eid
  where
    ti = typeInfo @a
    stmt = selectFromStmt @a byIdColumn
    eid = toSql idx

fromException :: SomeException -> PersistenceException
fromException ex = DatabaseError $ show ex

-- | This function retrieves all entities of type `a` that match some query criteria.
--   The function takes an HDBC connection and a `WhereClauseExpr` as parameters.
--   The type `a` is determined by the context of the function call.
--   The function returns a (possibly empty) list of all matching entities.
--   The `WhereClauseExpr` is typically constructed using a tiny query dsl based on infix operators.
select :: forall a. (Entity a) => Conn -> WhereClauseExpr -> IO (Either PersistenceException [a])
select conn whereClause = do
  -- print stmt
  eitherExRows <- tryPE $ quickQuery conn stmt values
  case eitherExRows of
    Left ex          -> return $ Left ex
    Right resultRows -> entitiesFromRows conn resultRows
  where
    stmt = selectFromStmt @a whereClause
    values = whereClauseValues whereClause

-- | This function retrieves the number of entities of type `a` that match some query criteria.
--   The function takes an HDBC connection and a `WhereClauseExpr` as parameters.
--   The type `a` is determined by the context of the function call.
--   The function returns the number of all matching entities.
--   The `WhereClauseExpr` is typically constructed using a tiny query dsl based on infix operators.
count :: forall a. (Entity a) => Conn -> WhereClauseExpr -> IO (Either PersistenceException Int)
count conn whereClause = do
  eitherExRows <- tryPE $ quickQuery conn stmt values
  case eitherExRows of
    Left ex          -> return $ Left ex
    Right resultRows -> return $ Right $ fromSql $ head $ head resultRows -- using head twice is safe here
  where
    stmt = countStmtFor @a whereClause
    values = whereClauseValues whereClause

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
upsert :: forall a. (Entity a) => Conn -> a -> IO (Either PersistenceException ())
upsert conn entity = do
  eitherExOrA <- try $ do
    row <- toRow conn entity
    _ <- quickQuery' conn (upsertStmtFor @a) (row <> row)
    commitIfAutoCommit conn
  case eitherExOrA of
    Left ex -> return $ Left $ fromException ex
    Right _ -> return $ Right ()

-- | A function that persists an entity to a database.
-- The function takes an HDBC connection and an entity as parameters.
-- The entity is either inserted or updated, depending on whether it already exists in the database.
-- The required SQL statements are generated dynamically using Haskell generics and reflection
-- deprecated: use upsert instead
{-# DEPRECATED persist "use upsert instead" #-}
persist :: forall a. (Entity a) => Conn -> a -> IO (Either PersistenceException ())
persist = upsert


-- | A function that commits a transaction if the connection is in auto commit mode.
--   The function takes an HDBC connection as parameter.
commitIfAutoCommit :: Conn -> IO ()
commitIfAutoCommit (Conn autoCommit conn) = when autoCommit $ commit conn

-- | A function that explicitely inserts an entity into a database.
--   The function takes an HDBC connection and an entity as parameters.
--   The entity, as retrieved from the database, is returned as a Right value if the entity was successfully inserted.
--   (this allows to handle automatically updated fields like auto-incremented primary keys)
--   If the entity could not be inserted, a Left value is returned, containing a PersistenceException.
insert :: forall a. (Entity a) => Conn -> a -> IO (Either PersistenceException a)
insert conn entity = do
  eitherExOrA <- try $ do
    row <- toRow conn entity
    [singleRow] <- quickQuery' conn (insertReturningStmtFor @a) (removeAutoIncIdField @a row)
    commitIfAutoCommit conn
    fromRow @a conn singleRow
  case eitherExOrA of
    Left ex -> return $ Left $ handleDuplicateInsert ex
    Right a -> return $ Right a

removeAutoIncIdField :: forall a. (Entity a) => [SqlValue] -> [SqlValue]
removeAutoIncIdField row =
  if autoIncrement @a
    then case maybeIdFieldIndex @a of
      Nothing      -> row
      Just idIndex -> take idIndex row ++ drop (idIndex + 1) row
    else row

handleDuplicateInsert :: SomeException -> PersistenceException
handleDuplicateInsert ex =
  if "UNIQUE constraint failed"
    `isInfixOf` show ex
    || "duplicate key value violates unique constraint"
    `isInfixOf` show ex
    then DuplicateInsert "Entity already exists in DB, use update instead"
    else fromException ex

tryPE :: IO a -> IO (Either PersistenceException a)
tryPE action = do
  eitherExResult <- try action
  case eitherExResult of
    Left ex      -> return $ Left $ fromException ex
    Right result -> return $ Right result

-- | A function that inserts a list of entities into a database.
--   The function takes an HDBC connection and a list of entities as parameters.
--   The insert-statement is compiled only once and then executed for each entity.
insertMany :: forall a. (Entity a) => Conn -> [a] -> IO (Either PersistenceException ())
insertMany conn entities = do
  eitherExUnit <- try $ do
    rows <- mapM (toRow conn) entities
    stmt <- prepare conn (insertStmtFor @a)
    executeMany stmt (map (removeAutoIncIdField @a) rows)
    commitIfAutoCommit conn
  case eitherExUnit of
    Left ex -> return $ Left $ handleDuplicateInsert ex
    Right _ -> return $ Right ()

-- | A function that explicitely updates an entity in a database.
--  The function takes an HDBC connection and an entity as parameters.
update :: forall a. (Entity a) => Conn -> a -> IO (Either PersistenceException ())
update conn entity = do
  eitherExUnit <- try $ do
    eid <- idValue conn entity
    row <- toRow conn entity
    rowcount <- run conn (updateStmtFor @a) (row ++ [eid])
    if rowcount == 0
      then return (Left (EntityNotFound (constructorName (typeInfo @a) ++ " " ++ show eid ++ " does not exist")))
      else do
        commitIfAutoCommit conn
        return $ Right ()
  case eitherExUnit of
    Left ex      -> return $ Left $ fromException ex
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
  commitIfAutoCommit conn

-- | A function that deletes an entity from a database.
--   The function takes an HDBC connection and an entity as parameters.
delete :: forall a. (Entity a) => Conn -> a -> IO (Either PersistenceException ())
delete conn entity = do
  eitherExRes <- try $ do
    eid <- idValue conn entity
    rowCount <- run conn (deleteStmtFor @a) [eid]
    if rowCount == 0
      then return (Left (EntityNotFound (constructorName (typeInfo @a) ++ " " ++ show eid ++ " does not exist")))
      else do
        commitIfAutoCommit conn
        return $ Right ()
  case eitherExRes of
    Left ex      -> return $ Left $ fromException ex
    Right result -> return result

-- | A function that deletes an entity from a database by its id.
--   The function takes an HDBC connection and an entity id as parameters.
deleteById :: forall a id. (Entity a, Convertible id SqlValue) => Conn -> id -> IO (Either PersistenceException ())
deleteById conn idx = do
  eitherExRes <- try $ do
    let eid = toSql idx
    rowCount <- run conn (deleteStmtFor @a) [eid]
    if rowCount == 0
      then return (Left (EntityNotFound (constructorName (typeInfo @a) ++ " " ++ show eid ++ " does not exist")))
      else do
        commitIfAutoCommit conn
        return $ Right ()
  case eitherExRes of
    Left ex      -> return $ Left $ fromException ex
    Right result -> return result

-- | A function that deletes a list of entities from a database by their ids.
--   The function takes an HDBC connection and a list of entity ids as parameters.
deleteManyById :: forall a id. (Entity a, Convertible id SqlValue) => Conn -> [id] -> IO (Either PersistenceException ())
deleteManyById conn ids = tryPE $ do
  stmt <- prepare conn (deleteStmtFor @a)
  executeMany stmt (map ((: []) . toSql) ids)
  commitIfAutoCommit conn

-- | A function that deletes a list of entities from a database.
--   The function takes an HDBC connection and a list of entities as parameters.
--   The delete-statement is compiled only once and then executed for each entity.
deleteMany :: forall a. (Entity a) => Conn -> [a] -> IO (Either PersistenceException ())
deleteMany conn entities = tryPE $ do
  eids <- mapM (idValue conn) entities
  stmt <- prepare conn (deleteStmtFor @a)
  executeMany stmt (map (: []) eids)
  commitIfAutoCommit conn

-- | set up a table for a given entity type. The table is dropped (if existing) and recreated.
--   The function takes an HDBC connection and a column type mapping as parameters.
setupTable :: forall a. (Entity a) => Conn -> ColumnTypeMapping -> IO ()
setupTable conn mapping = do
  runRaw conn $ dropTableStmtFor @a
  runRaw conn $ createTableStmtFor @a mapping
  commitIfAutoCommit conn

-- | A function that returns the primary key value of an entity as a SqlValue.
--   The function takes an HDBC connection and an entity as parameters.
idValue :: forall a. (Entity a) => Conn -> a -> IO SqlValue
idValue conn x = do
  sqlValues <- toRow conn x
  return (sqlValues !! idFieldIndex)
  where
    idFieldIndex = fieldIndex @a (idField @a)

-- | an alias for a simple quasiqouter
sql :: QuasiQuoter
sql = r

-- | These instances are needed to make the Convertible type class work with Enum types out of the box.
--   This is needed because the Convertible type class is used to convert SqlValues to Haskell types.
instance {-# OVERLAPS #-} forall a. (Enum a) => Convertible SqlValue a where
  safeConvert :: SqlValue -> ConvertResult a
  safeConvert = Right . toEnum . fromSql

instance {-# OVERLAPS #-} forall a. (Enum a) => Convertible a SqlValue where
  safeConvert :: a -> ConvertResult SqlValue
  safeConvert = Right . toSql . fromEnum
