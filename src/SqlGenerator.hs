module SqlGenerator
  ( insertStmtFor,
    updateStmtFor,
    selectStmtFor,
    deleteStmtFor,
    selectAllStmtFor,
  )
where

import           Data.Data (fromConstr)
import           Data.List (intercalate)
import           Entity
import           TypeInfo

-- | A function that returns an SQL insert statement for an entity. Type 'a' must be an instance of Data.
-- The function will use the field names of the data type to generate the column names in the insert statement.
-- The values of the fields will be used as the values in the insert statement.
-- Output example: INSERT INTO Person (id, name, age, address) VALUES (123456, "Alice", 25, "123 Main St");
insertStmtFor :: Entity a => a -> String
insertStmtFor x =
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

-- | A function that returns an SQL update statement for an entity. Type 'a' must be an instance of Entity.
updateStmtFor :: Entity a => a -> String
updateStmtFor x =
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
selectStmtFor :: forall a. (Entity a) => TypeInfo a -> String
selectStmtFor ti =
  "SELECT "
    ++ intercalate ", " (columnNamesFor x)
    ++ " FROM "
    ++ tableName x
    ++ " WHERE "
    ++ idColumn x
    ++ " = ?;"
  where
    x = fromConstr (typeConstructor ti) :: a

selectAllStmtFor :: forall a. (Entity a) => TypeInfo a -> String
selectAllStmtFor ti =
  "SELECT "
    ++ intercalate ", " (columnNamesFor x)
    ++ " FROM "
    ++ tableName x
    ++ ";"
  where
    x = fromConstr (typeConstructor ti) :: a

deleteStmtFor :: Entity a => a -> String
deleteStmtFor x =
  "DELETE FROM "
    ++ tableName x
    ++ " WHERE "
    ++ idColumn x
    ++ " = ?;"

