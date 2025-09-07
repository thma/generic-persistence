{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.GP.Query
  ( WhereClauseExpr(..),
    Field(..),
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
    between,
    in',
    isNull,
    not',
    params,
    sqlFun,
    allEntries,
    idColumn,
    byId,
    byIdColumn,
    orderBy,
    SortOrder (..),
    limit,
    limitOffset,
    NonEmpty (..),
    JoinType (..),
    JoinCondition (..),
    innerJoin,
    leftJoin,
    rightJoin,
    fullJoin,
    qualifiedField,
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
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Database.GP.Entity (Entity, columnNameFor, idFieldName)
import           Database.HDBC      (SqlValue, toSql)

data CompareOp = Eq | Gt | Lt | GtEq | LtEq | NotEq | Like

data Field = Field [String] String
  deriving (Show, Eq)

data JoinType = InnerJoin | LeftJoin | RightJoin | FullJoin
  deriving (Show, Eq)

data JoinCondition = JoinCondition
  { joinType :: JoinType,
    joinTable :: String,
    joinAlias :: Maybe String,
    leftField :: Field,
    rightField :: Field
  }
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
  | ByIdColumn
  | OrderBy WhereClauseExpr (NonEmpty (Field, SortOrder))
  | Limit WhereClauseExpr Int
  | LimitOffset WhereClauseExpr Int Int
  | Join WhereClauseExpr [JoinCondition]

data SortOrder = ASC | DESC
  deriving (Show)

field :: String -> Field
field = Field []

infixl 3 &&.

(&&.) :: WhereClauseExpr -> WhereClauseExpr -> WhereClauseExpr
(&&.) = And

infixl 2 ||.

(||.) :: WhereClauseExpr -> WhereClauseExpr -> WhereClauseExpr
(||.) = Or

infixl 4 =., >., <., >=., <=., <>., `like`, `between`, `in'`

(=.), (>.), (<.), (>=.), (<=.), (<>.), like :: (Convertible b SqlValue) => Field -> b -> WhereClauseExpr
a =. b = Where a Eq (toSql b)
a >. b = Where a Gt (toSql b)
a <. b = Where a Lt (toSql b)
a >=. b = Where a GtEq (toSql b)
a <=. b = Where a LtEq (toSql b)
a <>. b = Where a NotEq (toSql b)
a `like` b = Where a Like (toSql b)

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

byIdColumn :: WhereClauseExpr
byIdColumn = ByIdColumn

sqlFun :: String -> Field -> Field
sqlFun fun (Field funs name) = Field (fun : funs) name

infixl 1 `orderBy`

orderBy :: WhereClauseExpr -> NonEmpty (Field, SortOrder) -> WhereClauseExpr
orderBy = OrderBy

limit :: WhereClauseExpr -> Int -> WhereClauseExpr
limit = Limit

limitOffset :: WhereClauseExpr -> (Int, Int) -> WhereClauseExpr
limitOffset c (offset, lim) = LimitOffset c offset lim

whereClauseExprToSql :: forall a fn id. (Entity a fn id) => WhereClauseExpr -> String
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
whereClauseExprToSql ByIdColumn = idColumn @a ++ " = ?"
whereClauseExprToSql (OrderBy clause pairs) = whereClauseExprToSql @a clause ++ " ORDER BY " ++ renderedPairs pairs
  where
    renderedPairs :: NonEmpty (Field, SortOrder) -> String
    renderedPairs ne = intercalate ", " (NE.toList (NE.map (\(f, order) -> columnToSql @a f ++ " " ++ show order) ne))
whereClauseExprToSql (Limit clause x) = whereClauseExprToSql @a clause ++ " LIMIT " ++ show x
whereClauseExprToSql (LimitOffset clause offset lim) = whereClauseExprToSql @a clause ++ " LIMIT " ++ show lim ++ " OFFSET " ++ show offset
whereClauseExprToSql (Join clause _joins) = whereClauseExprToSql @a clause

opToSql :: CompareOp -> String
opToSql Eq    = "="
opToSql Gt    = ">"
opToSql Lt    = "<"
opToSql GtEq  = ">="
opToSql LtEq  = "<="
opToSql NotEq = "<>"
opToSql Like  = "LIKE"

columnToSql :: forall a fn id. (Entity a fn id) => Field -> String
columnToSql = expandFunctions @a

idColumn :: forall a fn id. (Entity a fn id) => String
idColumn = columnNameFor @a (idFieldName @a)

expandFunctions :: forall a fn id. (Entity a fn id) => Field -> String
expandFunctions (Field [] name) = columnNameFor @a name
expandFunctions (Field (f : fs) name) = f ++ "(" ++ expandFunctions @a (Field fs name) ++ ")"

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
whereClauseValues ByIdColumn = []
whereClauseValues (OrderBy clause _) = whereClauseValues clause
whereClauseValues (Limit clause _) = whereClauseValues clause
whereClauseValues (LimitOffset clause _ _) = whereClauseValues clause
whereClauseValues (Join clause _) = whereClauseValues clause

params :: Int -> [String]
params n = replicate n "?"

qualifiedField :: String -> String -> Field
qualifiedField table fieldName = Field [] (table ++ "." ++ fieldName)

innerJoin :: WhereClauseExpr -> String -> Maybe String -> Field -> Field -> WhereClauseExpr
innerJoin expr table alias left right = 
  Join expr [JoinCondition InnerJoin table alias left right]

leftJoin :: WhereClauseExpr -> String -> Maybe String -> Field -> Field -> WhereClauseExpr
leftJoin expr table alias left right = 
  Join expr [JoinCondition LeftJoin table alias left right]

rightJoin :: WhereClauseExpr -> String -> Maybe String -> Field -> Field -> WhereClauseExpr
rightJoin expr table alias left right = 
  Join expr [JoinCondition RightJoin table alias left right]

fullJoin :: WhereClauseExpr -> String -> Maybe String -> Field -> Field -> WhereClauseExpr
fullJoin expr table alias left right = 
  Join expr [JoinCondition FullJoin table alias left right]
