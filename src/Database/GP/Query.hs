{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.GP.Query
  ( WhereClauseExpr,
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
    between,
    in',
    isNull,
    not',
    params,
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


data CompareOp = Eq | Gt | Lt | GtEq | LtEq | NotEq | Like
  deriving (Show, Eq)

type FieldName = String

data WhereClauseExpr
  = Where FieldName CompareOp SqlValue
  | WhereBetween FieldName (SqlValue,SqlValue)
  | WhereIn FieldName [SqlValue]
  | WhereIsNull FieldName
  | And WhereClauseExpr WhereClauseExpr
  | Or WhereClauseExpr WhereClauseExpr
  | Not WhereClauseExpr
  deriving (Show, Eq)

infixl 3 &&.

(&&.) :: WhereClauseExpr -> WhereClauseExpr -> WhereClauseExpr
(&&.) = And

infixl 2 ||.

(||.) :: WhereClauseExpr -> WhereClauseExpr -> WhereClauseExpr
(||.) = Or

infixl 4 =., >., <., >=., <=., <>., `like`, `between`, `in'` 

(=.), (>.), (<.), (>=.), (<=.), (<>.), like :: (Convertible b SqlValue) => FieldName -> b -> WhereClauseExpr
a =. b = Where a Eq (toSql b)
a >. b  = Where a Gt (toSql b)
a <. b  = Where a Lt (toSql b)
a >=. b = Where a GtEq (toSql b)
a <=. b = Where a LtEq (toSql b)
a <>. b = Where a NotEq (toSql b)
a `like` b = Where a Like (toSql b)

between :: (Convertible a1 SqlValue, Convertible a2 SqlValue) => FieldName -> (a1, a2) -> WhereClauseExpr
a `between` (b,c) = WhereBetween a (toSql b, toSql c) 

in' :: (Convertible b SqlValue) => FieldName -> [b] -> WhereClauseExpr
a `in'` b = WhereIn a (map toSql b)

isNull :: FieldName -> WhereClauseExpr
isNull = WhereIsNull

not' :: WhereClauseExpr -> WhereClauseExpr
not' = Not

whereClauseExprToSql :: forall a. (Entity a) => WhereClauseExpr -> String
whereClauseExprToSql (Where fieldName op _) = column ++ " " ++ opToSql op ++ " ?"
  where
    column = columnNameFor @a fieldName
    
    opToSql :: CompareOp -> String
    opToSql Eq    = "="
    opToSql Gt    = ">"
    opToSql Lt    = "<"
    opToSql GtEq  = ">="
    opToSql LtEq  = "<="
    opToSql NotEq = "<>"
    opToSql Like  = "LIKE"
whereClauseExprToSql (And e1 e2) = "(" ++ whereClauseExprToSql @a e1 ++ ") AND (" ++ whereClauseExprToSql @a e2 ++ ")"
whereClauseExprToSql (Or e1 e2)  = "(" ++ whereClauseExprToSql @a e1 ++ ") OR (" ++ whereClauseExprToSql @a e2 ++ ")"
whereClauseExprToSql (Not e)     = "NOT (" ++ whereClauseExprToSql @a e ++ ")"
whereClauseExprToSql (WhereBetween fieldName (_v1,_v2)) = column ++ " BETWEEN ? AND ?"
  where
    column = columnNameFor @a fieldName
whereClauseExprToSql (WhereIn fieldName v) = column ++ " IN (" ++ args ++ ")"
  where
    column = columnNameFor @a fieldName
    args = intercalate ", " (params (length v))
whereClauseExprToSql (WhereIsNull fieldName) = column ++ " IS NULL"
  where
    column = columnNameFor @a fieldName

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