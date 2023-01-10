module SqlGenerator
  ( insertStmtFor
  , updateStmtFor
  , deleteStmtFor
  , selectStmtFor
  )
where

import           Data.Data             (Data)
import           Data.List             (intercalate)
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

-- UPDATE table_name
-- SET column1 = value1, column2 = value2, ...
-- WHERE condition;

updateStmtFor :: Data a => a -> String
updateStmtFor x =
  "UPDATE "
    ++ show (typeName $ typeInfo x)
    ++ " SET "
    ++ intercalate ", " updatePairs
    ++ " WHERE ID = "
    ++ fieldValueAsString x "id"
    ++ ";"
    where updatePairs = zipWith (\n v -> n ++ " = " ++ v) (fieldNames x) (fieldValues x)

-- SELECT column1, column2, ...
-- FROM table_name
-- WHERE ID=1;
selectStmtFor :: TypeInfo -> String -> String
selectStmtFor ti id =
  "SELECT "
  ++ intercalate ", " (fieldNamesFromTypeInfo ti)
  ++ " FROM " ++ show (typeName ti) ++ " WHERE ID = " ++ id ++ ";"

-- DELETE FROM Customers WHERE ID=1234;
deleteStmtFor :: Data a => a -> String
deleteStmtFor x =
  "DELETE FROM "
    ++ show (typeName $ typeInfo x)
    ++ " WHERE ID="
    ++ fieldValueAsString x "id"
    ++ ";"



