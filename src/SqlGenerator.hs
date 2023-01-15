module SqlGenerator
  ( insertStmtFor,
    updateStmtFor,
    deleteStmtFor,
    selectStmtFor,
    idColumn,
  )
where

import           Data.Char (toLower)
import           Data.Data (Data)
import           Data.List (intercalate)
import           TypeInfo

-- | A function that returns an SQL insert statement for an instance of type 'a'. Type 'a' must be an instance of Data.
-- The function will use the field names of the data type to generate the column names in the insert statement.
-- The values of the fields will be used as the values in the insert statement.
-- Output example: INSERT INTO Person (id, name, age, address) VALUES (123456, "Alice", 25, "123 Main St");
insertStmtFor :: Data a => a -> String
insertStmtFor x =
  "INSERT INTO "
    ++ show (typeName $ typeInfo x)
    ++ " ("
    ++ intercalate ", " (fieldNames x)
    ++ ") VALUES ("
    ++ intercalate ", " (fieldValues x)
    ++ ");"

updateStmtFor :: Data a => a -> String
updateStmtFor x =
  "UPDATE "
    ++ show (typeName $ typeInfo x)
    ++ " SET "
    ++ intercalate ", " updatePairs
    ++ " WHERE "
    ++ idColumn ti
    ++ " = "
    ++ fieldValueAsString x (idColumn ti)
    ++ ";"
  where
    updatePairs = zipWith (\n v -> n ++ " = " ++ v) (fieldNames x) (fieldValues x)
    ti = typeInfo x

selectStmtFor :: (Show id) => TypeInfo -> id -> String
selectStmtFor ti eid =
  "SELECT "
    ++ intercalate ", " (fieldNamesFromTypeInfo ti)
    ++ " FROM "
    ++ show (typeName ti)
    ++ " WHERE "
    ++ idColumn ti
    ++ " = "
    ++ show eid
    ++ ";"

deleteStmtFor :: Data a => a -> String
deleteStmtFor x =
  "DELETE FROM "
    ++ show (typeName $ typeInfo x)
    ++ " WHERE "
    ++ idColumn ti
    ++ " = "
    ++ fieldValueAsString x (idColumn ti)
    ++ ";"
  where
    ti = typeInfo x

-- "CREATE TABLE IF NOT EXISTS Person (personID INT PRIMARY KEY, name TEXT, age INT, address TEXT);"
{--
createTableStmtFor :: TypeInfo -> String
createTableStmtFor ti =
  "CREATE TABLE IF NOT EXISTS "
    ++ show (typeName ti)
    ++ " ("
    ++ intercalate ", " (zipWith (\n t -> n ++ " " ++ t) (fieldNamesFromTypeInfo ti) (fieldTypesFromTypeInfo ti))
    ++ ");"
--}

idColumn :: TypeInfo -> String
idColumn ti = map toLower (typeName ti) ++ "ID"
