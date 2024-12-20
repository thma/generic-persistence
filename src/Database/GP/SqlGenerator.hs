{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.GP.SqlGenerator
  ( insertStmtFor,
    insertReturningStmtFor,
    updateStmtFor,
    upsertStmtFor,
    selectFromStmt,
    countStmtFor,
    deleteStmtFor,
    createTableStmtFor,
    dropTableStmtFor,
    columnTypeFor,
    WhereClauseExpr,
    Field,
    field,
    whereClauseValues,
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
    byIdColumn,
    orderBy,
    SortOrder (..),
    limit,
    limitOffset,
    NonEmpty (..),
    Database (..),
    defaultSqliteMapping,
    defaultPostgresMapping,
    ColumnTypeMapping,
  )
where

import           Data.List          (intercalate)
import           Database.GP.Entity
import           Database.GP.Query

-- |
--  This module defines some basic SQL statements for Record Data Types that are instances of 'Entity'.
--  The SQL statements are generated using Haskell generics to provide compile time reflection capabilities.

-- | A function that returns an SQL insert statement for an entity. Type 'a' must be an instance of Data.
-- The function will use the field names of the data type to generate the column names in the insert statement.
-- The values of the fields will be used as the values in the insert statement.
-- Output example: INSERT INTO Person (id, name, age, address) VALUES (123456, "Alice", 25, "123 Main St");
insertStmtFor :: forall a id. Entity a id => String
insertStmtFor =
  "INSERT INTO "
    ++ tableName @a
    ++ " ("
    ++ intercalate ", " insertCols
    ++ ") VALUES ("
    ++ intercalate ", " (params (length insertCols))
    ++ ");"
  where
    insertCols = insertColumns @a

upsertStmtFor :: forall a id. Entity a id => String
upsertStmtFor =
  "INSERT INTO "
    ++ tableName @a
    ++ " ("
    ++ intercalate ", " insertCols
    ++ ") VALUES ("
    ++ intercalate ", " (params (length insertCols))
    ++ ") ON CONFLICT ("
    ++ idColumn @a
    ++ ") DO UPDATE SET "
    ++ intercalate ", " updatePairs
    ++ ";"
  where
    insertCols = columnNamesFor @a
    updatePairs = map (++ " = ?") insertCols

insertColumns :: forall a id . Entity a id => [String]
insertColumns = 
  if autoIncrement @a
    then filter (/= idColumn @a) columns
    else columns 
  where
    columns = columnNamesFor @a

insertReturningStmtFor :: forall a id. Entity a id => String
insertReturningStmtFor =
  "INSERT INTO "
    ++ tableName @a
    ++ " ("
    ++ intercalate ", " insertCols
    ++ ") VALUES ("
    ++ intercalate ", " (params (length insertCols))
    ++ ") RETURNING "
    ++ returnCols
    ++ ";"
  where
    insertCols = insertColumns @a
    returnCols = intercalate ", " (columnNamesFor @a)

columnNamesFor :: forall a id . Entity a id => [String]
columnNamesFor = map snd fieldColumnPairs
  where
    fieldColumnPairs = fieldsToColumns @a

-- | A function that returns an SQL update statement for an entity. Type 'a' must be an instance of Entity.
updateStmtFor :: forall a id . (Entity a id) => String
updateStmtFor =
  "UPDATE "
    ++ tableName @a
    ++ " SET "
    ++ intercalate ", " updatePairs
    ++ " WHERE "
    ++ idColumn @a
    ++ " = ?"
    ++ ";"
  where
    updatePairs = map (++ " = ?") (columnNamesFor @a)

-- | A function that returns an SQL select statement for an entity. Type 'a' must be an instance of Entity.
--   The function takes a where clause expression as parameter. This expression is used to filter the result set.
selectFromStmt :: forall a id. (Entity a id) => WhereClauseExpr -> String
selectFromStmt whereClauseExpr =
  "SELECT "
    ++ intercalate ", " (columnNamesFor @a)
    ++ " FROM "
    ++ tableName @a
    ++ " WHERE "
    ++ whereClauseExprToSql @a whereClauseExpr
    ++ ";"

-- | A function that returns an SQL count statement for an entity. Type 'a' must be an instance of Entity.
--   The function takes a where clause expression as parameter. This expression is used to filter the result set.
countStmtFor :: forall a id. (Entity a id) => WhereClauseExpr -> String
countStmtFor whereClauseExpr =
  "SELECT COUNT(*) FROM "
    ++ tableName @a
    ++ " WHERE "
    ++ whereClauseExprToSql @a whereClauseExpr
    ++ ";"

-- | A function that returns an SQL delete statement for an entity. Type 'a' must be an instance of Entity.
deleteStmtFor :: forall a id. (Entity a id) => String
deleteStmtFor =
  "DELETE FROM "
    ++ tableName @a
    ++ " WHERE "
    ++ idColumn @a
    ++ " = ?;"

-- | An enumeration of the supported database types. 
data Database = Postgres | SQLite

-- | A function that returns an SQL create table statement for an entity type. Type 'a' must be an instance of Entity.
createTableStmtFor :: forall a id. (Entity a id) => ColumnTypeMapping -> String
createTableStmtFor columnTypeMapping =
  "CREATE TABLE "
    ++ tableName @a
    ++ " ("
    ++ intercalate ", " (map (\(f, c) -> c ++ " " ++ columnTypeFor @a columnTypeMapping f ++ optionalPK f) (fieldsToColumns @a))
    ++ ");"
  where
    optionalPK f = if isIdField @a f then " PRIMARY KEY" else ""

isIdField :: forall a id. (Entity a id) => String -> Bool
isIdField f = f == idField @a

-- | A function that returns the column type for a field of an entity.
--   The function takes a column type mapping function as parameter.
--   This function is used to map Haskell field types to SQL column types.
columnTypeFor :: forall a id. (Entity a id) => ColumnTypeMapping -> String -> String
columnTypeFor columnTypeMapping fieldName = columnTypeMapping fType
  where
    fType = 
      if autoIncrement @a && isIdField @a fieldName 
        then "AUTOINCREMENT"
        else maybe "OTHER" show $ maybeFieldTypeFor @a fieldName

-- | A type alias for mapping a Haskell field type to a SQL column type.
--   this type can be used to define custom mappings for field types.
type ColumnTypeMapping = String -> String

-- | The default mapping for SQLite databases.
--   This mapping is used when no custom mapping is provided.
defaultSqliteMapping :: ColumnTypeMapping
defaultSqliteMapping = \case
  "AUTOINCREMENT" -> "INTEGER"
  "Int"    -> "INTEGER"
  "[Char]" -> "TEXT"
  "Double" -> "REAL"
  "Float"  -> "REAL"
  "Bool"   -> "INT"
  _        -> "TEXT"

-- | The default mapping for Postgres databases.
--   This mapping is used when no custom mapping is provided.
defaultPostgresMapping :: ColumnTypeMapping
defaultPostgresMapping = \case
  "AUTOINCREMENT" -> "serial"
  "Int"    -> "numeric"
  "[Char]" -> "varchar"
  "Double" -> "numeric"
  "Float"  -> "numeric"
  "Bool"   -> "boolean"
  _        -> "varchar"

-- | This function generates a DROP TABLE statement for an entity type.
dropTableStmtFor :: forall a id. (Entity a id) => String
dropTableStmtFor =
  "DROP TABLE IF EXISTS "
    ++ tableName @a
    ++ ";"
