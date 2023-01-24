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
  ++ intercalate ", " (columnNamesFor x)
  ++ ") VALUES ("
  ++ intercalate ", " (params x)
  ++ ");"


columnNamesFor :: Entity a => a -> [String]
columnNamesFor x = map (columnNameFor x) fields
  where
    ti = typeInfo x
    fields = fieldNames ti

params :: Entity a => a -> [String]
params x = replicate (length (fieldNames (typeInfo x))) "?"

-- | A function that returns an SQL update statement for an entity. Type 'a' must be an instance of Data.
preparedUpdateStmtFor :: Entity a => a -> String
preparedUpdateStmtFor x =
  "UPDATE "
    ++ tableName x
    ++ " SET "
    ++ intercalate ", " updatePairs
    ++ " WHERE "
    ++ idColumn x
    ++ " = ?"
    ++ ";"
  where
    updatePairs = map (++ " = ?") (columnNamesFor x)

idColumn :: Entity a => a -> String
idColumn x = columnNameFor x (idField x)

-- | A function that returns an SQL select statement for entity type `a` with primary key `id`.
preparedSelectStmtFor :: forall a . (Entity a) => TypeInfo a -> String
preparedSelectStmtFor ti =
  "SELECT "
    ++ intercalate ", " (columnNamesFor proxy)
    ++ " FROM "
    ++ tableName proxy
    ++ " WHERE "
    ++ idColumn proxy
    ++ " = ?;"
  where
    proxy = fromConstr (typeConstructor ti) :: a

selectAllStmtFor :: forall a . (Entity a) => TypeInfo a -> String
selectAllStmtFor ti =
  "SELECT "
    ++ intercalate ", " (columnNamesFor proxy)
    ++ " FROM "
    ++ tableName proxy
    ++ ";"
  where 
    proxy = fromConstr (typeConstructor ti) :: a


preparedDeleteStmtFor :: Entity a => a -> String
preparedDeleteStmtFor x =
  "DELETE FROM "
    ++ tableName x
    ++ " WHERE "
    ++ idColumn x
    ++ " = ?;"


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


