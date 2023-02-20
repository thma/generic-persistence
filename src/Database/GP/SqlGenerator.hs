{-# LANGUAGE AllowAmbiguousTypes #-}
module Database.GP.SqlGenerator
  ( insertStmtFor,
    updateStmtFor,
    selectStmtFor,
    deleteStmtFor,
    selectAllStmtFor,
    selectAllWhereStmtFor,
    createTableStmtFor,
    dropTableStmtFor,
  )
where

import           Data.List            (intercalate)
import           Database.GP.Entity

-- | A function that returns an SQL insert statement for an entity. Type 'a' must be an instance of Data.
-- The function will use the field names of the data type to generate the column names in the insert statement.
-- The values of the fields will be used as the values in the insert statement.
-- Output example: INSERT INTO Person (id, name, age, address) VALUES (123456, "Alice", 25, "123 Main St");
insertStmtFor :: forall a . Entity a => String
insertStmtFor =
  "INSERT INTO "
    ++ tableName @a
    ++ " ("
    ++ intercalate ", " columns
    ++ ") VALUES ("
    ++ intercalate ", " (params (length columns))
    ++ ");"
  where
    columns = columnNamesFor @a


columnNamesFor :: forall a . Entity a => [String]
columnNamesFor =  map snd fieldColumnPairs
  where
    fieldColumnPairs = fieldsToColumns @a


params :: Int -> [String]
params n = replicate n "?"

-- | A function that returns an SQL update statement for an entity. Type 'a' must be an instance of Entity.
updateStmtFor :: forall a .(Entity a) => String
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

idColumn :: forall a . (Entity a) => String
idColumn = columnNameFor @a (idField @a)

-- | A function that returns an SQL select statement for entity type `a` with primary key `id`.
selectStmtFor :: forall a. (Entity a) => String
selectStmtFor =
  "SELECT "
    ++ intercalate ", " (columnNamesFor @a)
    ++ " FROM "
    ++ tableName @a
    ++ " WHERE "
    ++ idColumn @a
    ++ " = ?;"

selectAllStmtFor :: forall a. (Entity a) => String
selectAllStmtFor =
  "SELECT "
    ++ intercalate ", " (columnNamesFor @a)
    ++ " FROM "
    ++ tableName @a
    ++ ";"

selectAllWhereStmtFor :: forall a. (Entity a) => String -> String
selectAllWhereStmtFor field =
  "SELECT "
    ++ intercalate ", " (columnNamesFor @a)
    ++ " FROM "
    ++ tableName @a
    ++ " WHERE "
    ++ column
    ++ " = ?;"
  where
    column = columnNameFor @a field

deleteStmtFor :: forall a . (Entity a) => String
deleteStmtFor =
  "DELETE FROM "
    ++ tableName @a
    ++ " WHERE "
    ++ idColumn @a
    ++ " = ?;"

createTableStmtFor :: forall a. (Entity a) => String
createTableStmtFor =
  "CREATE TABLE "
    ++ tableName @a
    ++ " ("
    ++ intercalate ", " (map (\(f,c) -> c ++ " " ++ columnTypeFor @a f ++ optionalPK f) (fieldsToColumns @a))
    ++ ");"
  where
    isIdField f = f == idField @a
    optionalPK f = if isIdField f then " PRIMARY KEY" else ""


-- | A function that returns the SQLite column type for a field of an entity.
-- TODO: have a switch to also support other databases.
columnTypeFor :: forall a. (Entity a) => String -> String
columnTypeFor field =
  case fType of
    "Int"    -> "INTEGER"
    "String" -> "TEXT"
    "Double" -> "REAL"
    "Float"  -> "REAL"
    "Bool"   -> "INT"
    _        -> "TEXT"
    where
      maybeFType = maybeFieldTypeFor @a field
      fType = maybe "OTHER" show maybeFType


dropTableStmtFor :: forall a. (Entity a) => String
dropTableStmtFor =
  "DROP TABLE IF EXISTS "
    ++ tableName @a
    ++ ";"
