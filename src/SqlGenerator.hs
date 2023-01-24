module SqlGenerator
  ( preparedInsertStmtFor,
    preparedUpdateStmtFor,
    preparedSelectStmtFor,
    preparedDeleteStmtFor,
    selectAllStmtFor,
  )
where

import           Data.List            (intercalate)
import           TypeInfo
import           Entity         
import GHC.Data.Maybe (expectJust)
import Data.Data (fromConstr)

-- | A function that returns an SQL insert statement for an entity. Type 'a' must be an instance of Data.
-- The function will use the field names of the data type to generate the column names in the insert statement.
-- The values of the fields will be used as the values in the insert statement.
-- Output example: INSERT INTO Person (id, name, age, address) VALUES (123456, "Alice", 25, "123 Main St");
preparedInsertStmtFor :: Entity a => a -> String
preparedInsertStmtFor x =
  "INSERT INTO "
  ++ tableName x
  ++ " ("
  ++ intercalate ", " colNames
  ++ ") VALUES ("
  ++ intercalate ", " params
  ++ ");"
  where
    ti = typeInfo x
    fNames = fieldNames ti
    colNames = map (columnNameFor x) fNames
    params = replicate (length fNames) "?"

-- | A function that returns an SQL update statement for an entity. Type 'a' must be an instance of Data.
preparedUpdateStmtFor :: Entity a => a -> String
preparedUpdateStmtFor x =
  "UPDATE "
    ++ tableName x
    ++ " SET "
    ++ intercalate ", " updatePairs
    ++ " WHERE "
    ++ idCol
    ++ " = ?"
    ++ ";"
  where
    ti = typeInfo x
    fNames = fieldNames ti
    colNames = map (columnNameFor x) fNames
    updatePairs = map (++ " = ?") colNames
    idx = idField x
    idCol = expectJust 
      ("preparedUpdateStmtFor: " ++ toString x ++ " has no column mapping for " ++ idx) 
      (maybeColumnNameFor x idx)


-- | A function that returns an SQL select statement for entity type `a` with primary key `id`.
preparedSelectStmtFor :: forall a . (Entity a) => TypeInfo a -> String
preparedSelectStmtFor ti =
  "SELECT "
    ++ intercalate ", " colNames
    ++ " FROM "
    ++ tableName proxy
    ++ " WHERE "
    ++ idCol
    ++ " = ?;"
  where
    proxy = fromConstr (typeConstructor ti) :: a
    tName = typeName ti
    fNames = fieldNames ti
    colNames = map (columnNameFor proxy) fNames
    idx = idField proxy
    idCol = expectJust 
      ("preparedSelectStmtFor: " ++ tName ++ " has no column mapping for " ++ idx) 
      (maybeColumnNameFor proxy idx)

selectAllStmtFor :: forall a . (Entity a) => TypeInfo a -> String
selectAllStmtFor ti =
  "SELECT "
    ++ intercalate ", " colNames
    ++ " FROM "
    ++ tableName proxy
    ++ ";"
  where 
    proxy = fromConstr (typeConstructor ti) :: a
    fNames = fieldNames ti
    colNames = map (columnNameFor proxy) fNames


preparedDeleteStmtFor :: Entity a => a -> String
preparedDeleteStmtFor x =
  "DELETE FROM "
    ++ tableName x
    ++ " WHERE "
    ++ idCol
    ++ " = ?;"
  where
    idx = idField x
    idCol = expectJust 
      ("preparedDeleteStmtFor: " ++ toString x ++ " has no column mapping for " ++ idx) 
      (maybeColumnNameFor x idx)

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


