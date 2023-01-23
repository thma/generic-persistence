{-# LANGUAGE DefaultSignatures #-}
module Entity 
  (
    Entity (..),
    idColumn
  ) where

import Data.Data ( Data )
import Database.HDBC ( SqlValue )
import RecordtypeReflection ( gFromRow, gToRow )    
import TypeInfo
import Data.Char (toLower)


class (Data a) => Entity a where
  fromRow :: [SqlValue] -> a
  toRow :: a -> [SqlValue]
  idField :: a -> String
  nameMapping :: a -> [(String, String)]

  default fromRow :: [SqlValue] -> a
  fromRow = gFromRow

  default toRow :: a -> [SqlValue]
  toRow = gToRow

  default idField :: a -> String
  idField = idColumn . typeInfo

  default nameMapping :: a -> [(String, String)]
  nameMapping x = zip (fieldNames (typeInfo x)) (fieldNames (typeInfo x))


idFieldName :: TypeInfo a -> String
idFieldName ti = map toLower (typeName ti) ++ "ID"

-- | A function that returns the name of the primary key column for a type 'a'.
--   By convention we are using the following name: convert the type name to lower case and append "ID".
idColumn :: TypeInfo a -> String
idColumn ti = map toLower (typeName ti) ++ "ID"

columnNameFor :: Entity a => a -> String -> Maybe String
columnNameFor x fieldName = lookup fieldName (nameMapping x)