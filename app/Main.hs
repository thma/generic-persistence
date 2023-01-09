{-# LANGUAGE DeriveDataTypeable#-}
{-# LANGUAGE ExtendedDefaultRules#-}
{-# LANGUAGE GADTs #-}
module Main (main) where

import Data.Data hiding (typeRep, IntRep, TypeRep)

import TypeInfo
import SqlGenerator
import Data.Dynamic --(toDyn)
import Type.Reflection
--import GHC.Base (RuntimeRep(..))
import GHC.Data.Maybe
import Control.Monad
import GHC.Types

-- https://stackoverflow.com/questions/46992740/how-to-specify-type-of-value-via-typerep
types :: [SomeTypeRep]
types = [SomeTypeRep (typeRep @Int), SomeTypeRep (typeRep @Double)]

strings :: [String]
strings = ["34", "76876.89"]

converted :: [Dynamic]
converted = expectJust "casting not possible" $ zipWithM convert types strings

convert :: SomeTypeRep -> String -> Maybe Dynamic
convert (SomeTypeRep rep) str
  | Just HRefl <- eqTypeRep rep (typeRep @Int)    = Just $ toDyn (read str :: Int)
  | Just HRefl <- eqTypeRep rep (typeRep @Double) = Just $ toDyn (read str :: Double)
  | Just HRefl <- eqTypeRep rep (typeRep @String) = Just $ toDyn str
  | otherwise = Nothing



-- Define a sample data type
data MyType = MyType Int String
  deriving (Data)
  
data MySum = MySum1 Int | MySum2 String
  deriving (Data)  
  
data MyProduct = MyProduct Int MySum
  deriving (Data)  

-- | A data type with several fields, using record syntax.
data Person = Person
  { id :: Int
  , name :: String
  , age :: Int
  , address :: String
  } deriving (Data, Show)


p = Person 123456 "Alice" 25 "123 Main St"

mt = MyType 1 "Hello"

ms = MySum1 1

ms2 = MySum2 "Hello"

mp :: MyProduct
mp = MyProduct 1 ms2

main :: IO ()
main = do
  let fields = fieldInfo p
  putStrLn "Fields:"
  mapM_ (\(FieldInfo name type_) -> putStrLn $ show name ++ ": " ++ show type_) fields
  print ""

  putStrLn $ insertStmtFor p
  putStrLn $ selectStmtFor (typeInfo p) "123456"
  putStrLn $ updateStmtFor p
  putStrLn $ deleteStmtFor p

  let types :: [SomeTypeRep]
      types = [SomeTypeRep (typeRep @Int), SomeTypeRep (typeRep @String), SomeTypeRep (typeRep @Int), SomeTypeRep (typeRep @String)]
      strings :: [String]
      strings = ["4712", "Bob Marley", "36", "Tuff Gong Studio, Kingston, Jamaica"]
      converted :: [Dynamic]
      converted = expectJust "casting not possible" $ zipWithM convert types strings

      bob = applyConstr (toConstr p) [toDyn (4711 :: Int), toDyn "Bob", toDyn (30 :: Int), toDyn "456 Main St"] :: Maybe Person
      rob = applyConstr (toConstr p) [toDyn (read "5656" :: Int), toDyn "Rob", toDyn (31 :: Int), toDyn "457 Main St"] :: Maybe Person
      bobM = applyConstr (toConstr p) converted :: Maybe Person
      x = fromConstrB (fromConstr (toConstr 6879879)) (toConstr (Just 1 )) :: Maybe Int

  print bob
  print rob
  print bobM
  print x
  let ctor = toConstr p
      dtype = constrType ctor
  print dtype

