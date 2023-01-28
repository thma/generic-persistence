module SqlGenerator
  ( insertStmtFor,
    updateStmtFor,
    selectStmtFor,
    deleteStmtFor,
    selectAllStmtFor,
    createTableStmtFor,
    dropTableStmtFor,
  )
where

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
    ++ intercalate ", " columns
    ++ ") VALUES ("
    ++ intercalate ", " (params (length columns))
    ++ ");"
  where
    columns = columnNamesFor x


columnNamesFor :: Entity a => a -> [String]
columnNamesFor x =  columns 
  where
    fields = fieldsToColumns x 
    columns = map snd fields

params :: Int -> [String]
params n = replicate n "?"

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
    x = evidenceFrom ti :: a

selectAllStmtFor :: forall a. (Entity a) => TypeInfo a -> String
selectAllStmtFor ti =
  "SELECT "
    ++ intercalate ", " (columnNamesFor x)
    ++ " FROM "
    ++ tableName x
    ++ ";"
  where
    x = evidenceFrom ti :: a

deleteStmtFor :: Entity a => a -> String
deleteStmtFor x =
  "DELETE FROM "
    ++ tableName x
    ++ " WHERE "
    ++ idColumn x
    ++ " = ?;"

createTableStmtFor :: forall a. (Entity a) => TypeInfo a -> String
createTableStmtFor ti =
  "CREATE TABLE "
    ++ tableName x
    ++ " ("
    ++ intercalate ", " (map (\(f,c) -> c ++ " " ++ columnTypeFor x f ++ optionalPK f) (fieldsToColumns x))
    ++ ");"
  where
    x = evidenceFrom ti :: a
    isIdField f = f == idField x
    optionalPK f = if isIdField f then " PRIMARY KEY" else ""
    
    
columnTypeFor :: forall a. (Entity a) => a -> String -> String
columnTypeFor x field = 
  case fType of
    "Int" -> "INT"
    "String" -> "TEXT"
    "Double" -> "REAL"
    "Float" -> "REAL"
    "Bool" -> "INT"
    _ -> "TEXT"
    where
      maybeFType = maybeFieldTypeFor x field
      fType = maybe "OTHER" show maybeFType


dropTableStmtFor :: forall a. (Entity a) => TypeInfo a -> String
dropTableStmtFor ti =
  "DROP TABLE IF EXISTS "
    ++ tableName x
    ++ ";"
  where
    x = evidenceFrom ti :: a