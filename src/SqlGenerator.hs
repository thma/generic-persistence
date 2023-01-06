module SqlGenerator
  ( insertStmtFor
  , updateStmtFor
  )
where

import           Data.Data             (Data, gmapQ, showConstr, toConstr)
import           Data.Generics.Aliases (extQ)
import           Data.List             (intercalate)
import           Data.Maybe
import           TypeInfo

--import Data.Generics.Text (gshow)

insertStmtFor :: Data a => a -> String
insertStmtFor x =
  "INSERT INTO "
    ++ show (typeName $ typeInfo x)
    ++ " ("
    ++ intercalate ", " fieldList
    ++ ") VALUES ("
    ++ intercalate ", " valueList
    ++ ");"
  where
    fieldList = map (fromMaybe (error "works only for Record Types with named fields") . fieldName) $ fieldInfo x
    valueList = gmapQ gshow x 

-- UPDATE table_name
-- SET column1 = value1, column2 = value2, ...
-- WHERE condition; 

updateStmtFor :: Data a => a -> String
updateStmtFor x =
  "UPDATE "
    ++ show (typeName $ typeInfo x)
    ++ " SET "
    ++ intercalate ", " fieldList
    ++ " WHERE "
    ++ intercalate ", " valueList
    ++ ";"
  where
    fieldList = map (fromMaybe (error "works only for Record Types with named fields") . fieldName) $ fieldInfo x
    valueList = gmapQ gshow x

-- SELECT column1, column2, ...
-- FROM table_name
-- WHERE ID=1;
selectStmtFor :: TypeInfo -> String -> String
selectStmtFor typeInfo id = "SELECT * FROM " ++ show (typeName typeInfo) ++ " WHERE ID=" ++ id ++ ";"

-- DELETE FROM Customers WHERE CustomerName='Alfreds Futterkiste';
deleteStmtFor :: Data a => a -> String
deleteStmtFor x =
  "DELETE FROM "
    ++ show (typeName $ typeInfo x)
    ++ " WHERE "
    ++ intercalate ", " valueList
    ++ ";"
  where
    fieldList = map (fromMaybe (error "works only for Record Types with named fields") . fieldName) $ fieldInfo x
    valueList = gmapQ gshow x

-- | Generic show: taken from syb package
gshow :: Data a => a -> String
gshow x = gshows x ""

-- | Generic shows. code was modified from syb Data.Generics.Text
gshows :: Data a => a -> ShowS
gshows =
  ( \t ->
      (showString . showConstr . toConstr $ t)
        . (foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t)
  )
    `extQ` (shows :: String -> ShowS)
