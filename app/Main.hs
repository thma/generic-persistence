{-# LANGUAGE DeriveDataTypeable#-}
module Main (main) where

import Data.Data

import TypeInfo
import SqlGenerator
--import Data.List (intercalate, elemIndex)
import Data.Dynamic (toDyn)
--import Control.Monad.Trans.State.Lazy
--import Control.Monad.Trans.Class (lift)


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

  let bob = applyConstr (toConstr p) [toDyn (4711 :: Int), toDyn "Bob", toDyn (30 :: Int), toDyn "456 Main St"] :: Maybe Person
  print bob
  let ctor = toConstr p
      dtype = constrType ctor
  print dtype

