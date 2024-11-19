{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.GP.SqlGenerator
  ( insertStmtFor,
    insertReturningStmtFor,
    updateStmtFor,
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
insertStmtFor :: forall a. Entity a => String
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

insertColumns :: forall a. Entity a => [String]
insertColumns = 
  if autoIncrement @a
    then filter (/= idColumn @a) columns
    else columns 
  where
    columns = columnNamesFor @a

insertReturningStmtFor :: forall a. Entity a => String
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

columnNamesFor :: forall a. Entity a => [String]
columnNamesFor = map snd fieldColumnPairs
  where
    fieldColumnPairs = fieldsToColumns @a

-- | A function that returns an SQL update statement for an entity. Type 'a' must be an instance of Entity.
updateStmtFor :: forall a. (Entity a) => String
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
selectFromStmt :: forall a. (Entity a) => WhereClauseExpr -> String
selectFromStmt whereClauseExpr =
  "SELECT "
    ++ intercalate ", " (columnNamesFor @a)
    ++ " FROM "
    ++ tableName @a
    ++ " WHERE "
    ++ whereClauseExprToSql @a whereClauseExpr
    ++ ";"

countStmtFor :: forall a. (Entity a) => WhereClauseExpr -> String
countStmtFor whereClauseExpr =
  "SELECT COUNT(*) FROM "
    ++ tableName @a
    ++ " WHERE "
    ++ whereClauseExprToSql @a whereClauseExpr
    ++ ";"

deleteStmtFor :: forall a. (Entity a) => String
deleteStmtFor =
  "DELETE FROM "
    ++ tableName @a
    ++ " WHERE "
    ++ idColumn @a
    ++ " = ?;"

-- | An enumeration of the supported database types. 
data Database = Postgres | SQLite

createTableStmtFor :: forall a. (Entity a) => ColumnTypeMapping -> String
createTableStmtFor columnTypeMapping =
  "CREATE TABLE "
    ++ tableName @a
    ++ " ("
    ++ intercalate ", " (map (\(f, c) -> c ++ " " ++ columnTypeFor @a columnTypeMapping f ++ optionalPK f) (fieldsToColumns @a))
    ++ ");"
  where
    optionalPK f = if isIdField @a f then " PRIMARY KEY" else ""

isIdField :: forall a. (Entity a) => String -> Bool
isIdField f = f == idField @a

-- | A function that returns the column type for a field of an entity.
-- TODO: Support other databases than just SQLite and Postgres.
columnTypeFor :: forall a. (Entity a) => ColumnTypeMapping -> String -> String
columnTypeFor columnTypeMapping fieldName = columnTypeMapping fType
  where
    fType = 
      if isIdField @a fieldName 
        then "primaryKey"
        else maybe "OTHER" show $ maybeFieldTypeFor @a fieldName


type ColumnTypeMapping = String -> String

defaultSqliteMapping :: ColumnTypeMapping
defaultSqliteMapping = \case
  "primaryKey" -> "INTEGER"
  "Int"    -> "INTEGER"
  "[Char]" -> "TEXT"
  "Double" -> "REAL"
  "Float"  -> "REAL"
  "Bool"   -> "INT"
  _        -> "TEXT"

defaultPostgresMapping :: ColumnTypeMapping
defaultPostgresMapping = \case
  "primaryKey" -> "serial"
  "Int"    -> "numeric"
  "[Char]" -> "varchar"
  "Double" -> "numeric"
  "Float"  -> "numeric"
  "Bool"   -> "boolean"
  _        -> "varchar"

dropTableStmtFor :: forall a. (Entity a) => String
dropTableStmtFor =
  "DROP TABLE IF EXISTS "
    ++ tableName @a
    ++ ";"
