module SqlGenerator
  ( preparedInsertStmtFor,
    preparedUpdateStmtFor,
    preparedSelectStmtFor,
    preparedDeleteStmtFor,
    selectAllStmtFor,
  )
where

import           Data.Data            (Data)
import           Data.List            (intercalate)
import           TypeInfo    
import           Entity         

-- | A function that returns an SQL insert statement for an entity. Type 'a' must be an instance of Data.
-- The function will use the field names of the data type to generate the column names in the insert statement.
-- The values of the fields will be used as the values in the insert statement.
-- Output example: INSERT INTO Person (id, name, age, address) VALUES (123456, "Alice", 25, "123 Main St");
preparedInsertStmtFor :: Data a => a -> String
preparedInsertStmtFor x =
  "INSERT INTO "
  ++ typeName ti
  ++ " ("
  ++ intercalate ", " fNames
  ++ ") VALUES ("
  ++ intercalate ", " params
  ++ ");"
  where
    ti = typeInfo x
    fNames = fieldNames ti
    params = replicate (length fNames) "?"

-- | A function that returns an SQL update statement for an entity. Type 'a' must be an instance of Data.
preparedUpdateStmtFor :: Data a => a -> String
preparedUpdateStmtFor x =
  "UPDATE "
    ++ typeName ti
    ++ " SET "
    ++ intercalate ", " updatePairs
    ++ " WHERE "
    ++ idColumn ti
    ++ " = ?"
    ++ ";"
  where
    ti = typeInfo x
    fNames = fieldNames ti
    updatePairs = map (++ " = ?") fNames


-- | A function that returns an SQL select statement for entity type `a` with primary key `id`.
preparedSelectStmtFor :: TypeInfo a -> String
preparedSelectStmtFor ti =
  "SELECT "
    ++ intercalate ", " (fieldNames ti)
    ++ " FROM "
    ++ typeName ti
    ++ " WHERE "
    ++ idColumn ti
    ++ " = ?;"

selectAllStmtFor :: TypeInfo a -> String
selectAllStmtFor ti =
  "SELECT "
    ++ intercalate ", " (fieldNames ti)
    ++ " FROM "
    ++ typeName ti
    ++ ";"

preparedDeleteStmtFor :: Data a => a -> String
preparedDeleteStmtFor x =
  "DELETE FROM "
    ++ show (typeName ti)
    ++ " WHERE "
    ++ idColumn ti
    ++ " = ?;"
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


