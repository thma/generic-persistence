{-# LANGUAGE DeriveDataTypeable#-}
{-# LANGUAGE ExtendedDefaultRules#-}
{-# LANGUAGE GADTs #-}
module Main (main) where

import Data.Data hiding (typeRep, IntRep, TypeRep)

import TypeInfo
import RecordtypeReflection
import SqlGenerator
import Data.Dynamic --(toDyn)
import Type.Reflection
import GHC.Data.Maybe (expectJust)
import Control.Monad

-- | A data type with several fields, using record syntax.
data Person = Person
  { id :: !Int
  , name :: !String
  , age :: !Int
  , address :: !String
  } deriving (Data, Show)

p = Person 123456 "Alice" 25 "123 Main St"

proxy = Proxy :: Proxy Person

main :: IO ()
main = do
  let fields = fieldInfo p
  putStrLn "Fields:"
  mapM_ (\(FieldInfo name constr type_) -> putStrLn $ show name ++ ": " ++ show type_) fields
  print ""

  putStrLn $ insertStmtFor p
  putStrLn $ selectStmtFor (typeInfo p) "123456"
  putStrLn $ updateStmtFor p
  putStrLn $ deleteStmtFor p

  let 
      strings :: [String]
      strings = ["4712", "Bob Marley", "361", "Tuff Gong Studio, Kingston, Jamaica"]
      
      bobMa = (buildFromRecord (typeInfo @Person) strings :: Maybe Person)

  print bobMa


