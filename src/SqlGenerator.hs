module SqlGenerator
  ( insertStmtFor,
  )
where

import           Data.Data             (Constr, Data, TypeRep, dataTypeConstrs,
                                        dataTypeOf, gmapQ, showConstr, toConstr,
                                        typeOf)
import           Data.Generics.Aliases (extQ)
import           Data.List             (intercalate)
import           Data.Maybe
import           TypeInfo

-- import Data.Generics.Text (gshow)

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
