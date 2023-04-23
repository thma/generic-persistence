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
    orderBy,
    SortOrder (..),
    limit,
    limitOffset,
  )
where

{--
  This module defines a DSL for building SQL SELECT WHERE clauses.
  The DSL provides query operators like =., >., <. for the most common SQL comparison operators.
  The DSL also provides the ability to combine WHERE clauses using the &&. and ||. operators.
  And to negate a where clause using the not' operator.
  The DSL is used in the `select` function of the Database.GP.GenericPersistence module.
  Example:
  thirtySomethings <- select conn (field "age" `between` (30 :: Int, 39 :: Int))
--}

import           Data.Convertible   (Convertible)
import           Data.List          (intercalate)
import           Database.GP.Entity (Entity, columnNameFor, idField)
import           Database.HDBC      (SqlValue, toSql)

data CompareOp = Eq | Gt | Lt | GtEq | LtEq | NotEq | Like | Contains
  deriving (Show, Eq)

data Field = Field [String] String
  deriving (Show, Eq)

data WhereClauseExpr
  = Where Field CompareOp SqlValue
  | WhereBetween Field (SqlValue, SqlValue)
  | WhereIn Field [SqlValue]
  | WhereIsNull Field
  | And WhereClauseExpr WhereClauseExpr
  | Or WhereClauseExpr WhereClauseExpr
  | Not WhereClauseExpr
  | All
  | ById SqlValue
  | OrderBy WhereClauseExpr [(Field, SortOrder)]
  | Limit WhereClauseExpr Int
  | LimitOffset WhereClauseExpr Int Int
  deriving (Show, Eq)

data SortOrder = ASC | DESC deriving (Show, Eq)

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
a >. b = Where a Gt (toSql b)
a <. b = Where a Lt (toSql b)
a >=. b = Where a GtEq (toSql b)
a <=. b = Where a LtEq (toSql b)
a <>. b = Where a NotEq (toSql b)
a `like` b = Where a Like (toSql b)

contains :: Convertible a SqlValue => Field -> a -> WhereClauseExpr
a `contains` b = Where a Contains (toSql b)

between :: (Convertible a1 SqlValue, Convertible a2 SqlValue) => Field -> (a1, a2) -> WhereClauseExpr
a `between` (b, c) = WhereBetween a (toSql b, toSql c)

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
sqlFun fun (Field funs name) = Field (fun : funs) name

infixl 1 `orderBy`

orderBy :: WhereClauseExpr -> [(Field, SortOrder)] -> WhereClauseExpr
orderBy = OrderBy

limit :: WhereClauseExpr -> Int -> WhereClauseExpr
limit = Limit 

limitOffset :: WhereClauseExpr -> (Int, Int) -> WhereClauseExpr
limitOffset c (offset, lim) = LimitOffset c offset lim


whereClauseExprToSql :: forall a. (Entity a) => WhereClauseExpr -> String
whereClauseExprToSql (Where f op _) = columnToSql @a f ++ " " ++ opToSql op ++ " ?"
whereClauseExprToSql (And e1 e2) = "(" ++ whereClauseExprToSql @a e1 ++ ") AND (" ++ whereClauseExprToSql @a e2 ++ ")"
whereClauseExprToSql (Or e1 e2) = "(" ++ whereClauseExprToSql @a e1 ++ ") OR (" ++ whereClauseExprToSql @a e2 ++ ")"
whereClauseExprToSql (Not (WhereIsNull f)) = columnToSql @a f ++ " IS NOT NULL"
whereClauseExprToSql (Not e) = "NOT (" ++ whereClauseExprToSql @a e ++ ")"
whereClauseExprToSql (WhereBetween f (_v1, _v2)) = columnToSql @a f ++ " BETWEEN ? AND ?"
whereClauseExprToSql (WhereIn f v) = columnToSql @a f ++ " IN (" ++ args ++ ")"
  where
    args = intercalate ", " (params (length v))
whereClauseExprToSql (WhereIsNull f) = columnToSql @a f ++ " IS NULL"
whereClauseExprToSql All = "1=1"
whereClauseExprToSql (ById _eid) = idColumn @a ++ " = ?"
whereClauseExprToSql (OrderBy clause pairs) = whereClauseExprToSql @a clause ++ " ORDER BY " ++ renderedPairs pairs
  where
    renderedPairs [] = ""
    renderedPairs [(f,order)] = columnToSql @a f ++ " " ++ show order
    renderedPairs (hd:tl) = renderedPairs [hd] ++ ", " ++ renderedPairs tl
whereClauseExprToSql (Limit clause x) = whereClauseExprToSql @a clause ++ " LIMIT " ++ show x
whereClauseExprToSql (LimitOffset clause offset lim) = whereClauseExprToSql @a clause ++ " LIMIT " ++ show lim ++ " OFFSET " ++ show offset 
    
opToSql :: CompareOp -> String
opToSql Eq       = "="
opToSql Gt       = ">"
opToSql Lt       = "<"
opToSql GtEq     = ">="
opToSql LtEq     = "<="
opToSql NotEq    = "<>"
opToSql Like     = "LIKE"
opToSql Contains = "CONTAINS"

columnToSql :: forall a. (Entity a) => Field -> String
columnToSql f = expandFunctions f $ columnNameFor @a (getName f)

idColumn :: forall a. (Entity a) => String
idColumn = columnNameFor @a (idField @a)

expandFunctions :: Field -> String -> String
expandFunctions (Field [] _name) col = col
expandFunctions (Field (f : fs) name) col = f ++ "(" ++ expandFunctions (Field fs name) col ++ ")"

whereClauseValues :: WhereClauseExpr -> [SqlValue]
whereClauseValues (Where _ _ v) = [toSql v]
whereClauseValues (And e1 e2) = whereClauseValues e1 ++ whereClauseValues e2
whereClauseValues (Or e1 e2) = whereClauseValues e1 ++ whereClauseValues e2
whereClauseValues (Not e) = whereClauseValues e
whereClauseValues (WhereBetween _ (v1, v2)) = [toSql v1, toSql v2]
whereClauseValues (WhereIn _ v) = map toSql v
whereClauseValues (WhereIsNull _) = []
whereClauseValues All = []
whereClauseValues (ById eid) = [toSql eid]
whereClauseValues (OrderBy clause _) = whereClauseValues clause
whereClauseValues (Limit clause _) = whereClauseValues clause
whereClauseValues (LimitOffset clause _ _) = whereClauseValues clause

params :: Int -> [String]
params n = replicate n "?"
