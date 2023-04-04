{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.GP.Query
  ( WhereClauseExpr,
    Field,
    field,
    whereClauseExprToSql,
    whereClauseValues,
    (&&.),
    (||.),
    (=.),
    (>.),
    (<.),
    (>=.),
    (<=.),
    (<>.),
    like,
    contains,
    between,
    in',
    isNull,
    not',
    params,
    sqlFun,
    allEntries,
    idColumn,
    byId,
  )
where

{--
  This module defines a DSL for building SQL where clauses.
  The DSL provides query operators like ==., >., <. for the most common SQL comparison operators.
  The DSL also provides the ability to combine where clauses using the &&. and ||. operators.
  And to negate a where clause using the !. operator.
  The DSL is used in the retrieveWhere function of the Database.GP.GenericPersistence module.
  Example:
  thirtySomethings <- select conn (field "age" `between` (30 :: Int, 40 :: Int))
--}

import           Data.Convertible   (Convertible)
import           Database.GP.Entity (Entity, columnNameFor, idField)
import           Database.HDBC      (SqlValue, toSql)
import Data.List (intercalate)


data CompareOp = Eq | Gt | Lt | GtEq | LtEq | NotEq | Like | Contains
  deriving (Show, Eq)

data Field = Field [String] String
  deriving (Show, Eq)

data WhereClauseExpr
  = Where Field CompareOp SqlValue
  | WhereBetween Field (SqlValue,SqlValue)
  | WhereIn Field [SqlValue]
  | WhereIsNull Field
  | And WhereClauseExpr WhereClauseExpr
  | Or WhereClauseExpr WhereClauseExpr
  | Not WhereClauseExpr
  | All
  | ById SqlValue
  deriving (Show, Eq)

field :: String -> Field
field = Field []

getName :: Field -> String
getName (Field _fns n) = n

infixl 3 &&.

(&&.) :: WhereClauseExpr -> WhereClauseExpr -> WhereClauseExpr
(&&.) = And

infixl 2 ||.

(||.) :: WhereClauseExpr -> WhereClauseExpr -> WhereClauseExpr
(||.) = Or

infixl 4 =., >., <., >=., <=., <>., `like`, `between`, `in'`, `contains`

(=.), (>.), (<.), (>=.), (<=.), (<>.), like :: (Convertible b SqlValue) => Field -> b -> WhereClauseExpr
a =. b = Where a Eq (toSql b)
a >. b  = Where a Gt (toSql b)
a <. b  = Where a Lt (toSql b)
a >=. b = Where a GtEq (toSql b)
a <=. b = Where a LtEq (toSql b)
a <>. b = Where a NotEq (toSql b)
a `like` b = Where a Like (toSql b)

contains :: Convertible a SqlValue => Field -> a -> WhereClauseExpr
a `contains` b = Where a Contains (toSql b)

between :: (Convertible a1 SqlValue, Convertible a2 SqlValue) => Field -> (a1, a2) -> WhereClauseExpr
a `between` (b,c) = WhereBetween a (toSql b, toSql c) 

in' :: (Convertible b SqlValue) => Field -> [b] -> WhereClauseExpr
a `in'` b = WhereIn a (map toSql b)

isNull :: Field -> WhereClauseExpr
isNull = WhereIsNull

not' :: WhereClauseExpr -> WhereClauseExpr
not' = Not

allEntries :: WhereClauseExpr
allEntries = All

byId :: (Convertible a SqlValue) => a -> WhereClauseExpr
byId = ById . toSql

sqlFun :: String -> Field -> Field
sqlFun fun (Field funs name) = Field (fun:funs) name


whereClauseExprToSql :: forall a. (Entity a) => WhereClauseExpr -> String
whereClauseExprToSql (Where f op _) = column ++ " " ++ opToSql op ++ " ?"
  where
    column = expandFunctions f $ columnNameFor @a (getName f)
    
    opToSql :: CompareOp -> String
    opToSql Eq    = "="
    opToSql Gt    = ">"
    opToSql Lt    = "<"
    opToSql GtEq  = ">="
    opToSql LtEq  = "<="
    opToSql NotEq = "<>"
    opToSql Like  = "LIKE"
    opToSql Contains = "CONTAINS"
whereClauseExprToSql (And e1 e2) = "(" ++ whereClauseExprToSql @a e1 ++ ") AND (" ++ whereClauseExprToSql @a e2 ++ ")"
whereClauseExprToSql (Or e1 e2)  = "(" ++ whereClauseExprToSql @a e1 ++ ") OR (" ++ whereClauseExprToSql @a e2 ++ ")"
whereClauseExprToSql (Not e)     = "NOT (" ++ whereClauseExprToSql @a e ++ ")"
whereClauseExprToSql (WhereBetween f (_v1,_v2)) = column ++ " BETWEEN ? AND ?"
  where
    column = expandFunctions f $ columnNameFor @a (getName f)
whereClauseExprToSql (WhereIn f v) = column ++ " IN (" ++ args ++ ")"
  where
    column = expandFunctions f $ columnNameFor @a (getName f)
    args = intercalate ", " (params (length v))
whereClauseExprToSql (WhereIsNull f) = column ++ " IS NULL"
  where
    column = expandFunctions f $ columnNameFor @a (getName f)
whereClauseExprToSql All = "1=1"
whereClauseExprToSql (ById _eid) = column ++ " = ?"
  where
    column = idColumn @a

idColumn :: forall a. (Entity a) => String
idColumn = columnNameFor @a (idField @a)

expandFunctions :: Field -> String -> String
expandFunctions (Field [] _name) col = col
expandFunctions (Field (f:fs) name) col = f ++ "(" ++ expandFunctions (Field fs name) col ++ ")"

whereClauseValues :: WhereClauseExpr -> [SqlValue]
whereClauseValues (Where _ _ v) = [toSql v]
whereClauseValues (And e1 e2)   = whereClauseValues e1 ++ whereClauseValues e2
whereClauseValues (Or e1 e2)    = whereClauseValues e1 ++ whereClauseValues e2
whereClauseValues (Not e)       = whereClauseValues e
whereClauseValues (WhereBetween _ (v1,v2)) = [toSql v1, toSql v2]
whereClauseValues (WhereIn _ v) = map toSql v
whereClauseValues (WhereIsNull _) = []
whereClauseValues All = []
whereClauseValues (ById eid) = [toSql eid]


params :: Int -> [String]
params n = replicate n "?"