{-# LANGUAGE DeriveDataTypeable#-}
{-# LANGUAGE ExtendedDefaultRules#-}
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

  let bob = applyConstr (toConstr p) [toDyn 4711, toDyn "Bob", toDyn 30, toDyn "456 Main St"] :: Maybe Person
      rob = applyConstr (toConstr p) [toDyn (read "5656" :: Int), toDyn "Rob", toDyn (31 :: Int), toDyn "457 Main St"] :: Maybe Person
      x = fromConstrB (fromConstr (toConstr 6879879)) (toConstr (Just 1 )) :: Maybe Int

  print bob
  print rob
  print x
  let ctor = toConstr p
      dtype = constrType ctor
  print dtype

