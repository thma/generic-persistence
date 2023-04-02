{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.GP.Query
  ( WhereClauseExpr,
    FieldName,
    fieldName,
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
  )
where

{--
  This module defines a DSL for building SQL where clauses.
  The DSL provides query operators like ==., >., <. for the most common SQL comparison operators.
  The DSL also provides the ability to combine where clauses using the &&. and ||. operators.
  And to negate a where clause using the !. operator.
  The DSL is used in the retrieveWhere function of the Database.GP.GenericPersistence module.
  Example:
  boomers <- retrieveWhere conn ("age" >. (30 :: Int))
--}

import           Data.Convertible   (Convertible)
import           Database.GP.Entity (Entity, columnNameFor)
import           Database.HDBC      (SqlValue, toSql)
import Data.List (intercalate)


data CompareOp = Eq | Gt | Lt | GtEq | LtEq | NotEq | Like | Contains
  deriving (Show, Eq)

data FieldName = FieldName [String] String
  deriving (Show, Eq)

data WhereClauseExpr
  = Where FieldName CompareOp SqlValue
  | WhereBetween FieldName (SqlValue,SqlValue)
  | WhereIn FieldName [SqlValue]
  | WhereIsNull FieldName
  | And WhereClauseExpr WhereClauseExpr
  | Or WhereClauseExpr WhereClauseExpr
  | Not WhereClauseExpr
  deriving (Show, Eq)

fieldName :: String -> FieldName
fieldName = FieldName []

getName :: FieldName -> String
getName (FieldName _fns n) = n

infixl 3 &&.

(&&.) :: WhereClauseExpr -> WhereClauseExpr -> WhereClauseExpr
(&&.) = And

infixl 2 ||.

(||.) :: WhereClauseExpr -> WhereClauseExpr -> WhereClauseExpr
(||.) = Or

infixl 4 =., >., <., >=., <=., <>., `like`, `between`, `in'`, `contains`

(=.), (>.), (<.), (>=.), (<=.), (<>.), like :: (Convertible b SqlValue) => FieldName -> b -> WhereClauseExpr
a =. b = Where a Eq (toSql b)
a >. b  = Where a Gt (toSql b)
a <. b  = Where a Lt (toSql b)
a >=. b = Where a GtEq (toSql b)
a <=. b = Where a LtEq (toSql b)
a <>. b = Where a NotEq (toSql b)
a `like` b = Where a Like (toSql b)

contains :: Convertible a SqlValue => FieldName -> a -> WhereClauseExpr
a `contains` b = Where a Contains (toSql b)

between :: (Convertible a1 SqlValue, Convertible a2 SqlValue) => FieldName -> (a1, a2) -> WhereClauseExpr
a `between` (b,c) = WhereBetween a (toSql b, toSql c) 

in' :: (Convertible b SqlValue) => FieldName -> [b] -> WhereClauseExpr
a `in'` b = WhereIn a (map toSql b)

isNull :: FieldName -> WhereClauseExpr
isNull = WhereIsNull

not' :: WhereClauseExpr -> WhereClauseExpr
not' = Not

sqlFun :: String -> FieldName -> FieldName
sqlFun fun (FieldName funs name) = FieldName (fun:funs) name

whereClauseExprToSql :: forall a. (Entity a) => WhereClauseExpr -> String
whereClauseExprToSql (Where field op _) = column ++ " " ++ opToSql op ++ " ?"
  where
    column = expandFunctions field $ columnNameFor @a (getName field)
    
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
whereClauseExprToSql (WhereBetween field (_v1,_v2)) = column ++ " BETWEEN ? AND ?"
  where
    column = expandFunctions field $ columnNameFor @a (getName field)
whereClauseExprToSql (WhereIn field v) = column ++ " IN (" ++ args ++ ")"
  where
    column = expandFunctions field $ columnNameFor @a (getName field)
    args = intercalate ", " (params (length v))
whereClauseExprToSql (WhereIsNull field) = column ++ " IS NULL"
  where
    column = expandFunctions field $ columnNameFor @a (getName field)

expandFunctions :: FieldName -> String -> String
expandFunctions (FieldName [] _name) col = col
expandFunctions (FieldName (f:fs) name) col = f ++ "(" ++ expandFunctions (FieldName fs name) col ++ ")"

whereClauseValues :: WhereClauseExpr -> [SqlValue]
whereClauseValues (Where _ _ v) = [toSql v]
whereClauseValues (And e1 e2)   = whereClauseValues e1 ++ whereClauseValues e2
whereClauseValues (Or e1 e2)    = whereClauseValues e1 ++ whereClauseValues e2
whereClauseValues (Not e)       = whereClauseValues e
whereClauseValues (WhereBetween _ (v1,v2)) = [toSql v1, toSql v2]
whereClauseValues (WhereIn _ v) = map toSql v
whereClauseValues (WhereIsNull _) = []


params :: Int -> [String]
params n = replicate n "?"