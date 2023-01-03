module Main where

import Data.Data
import TypeInfo

-- Define a sample data type
data MyType = MyType Int String
  deriving (Data)
  
data MySum = MySum1 Int | MySum2 String
  deriving (Data)  
  
data MyProduct = MyProduct Int MySum
  deriving (Data)  

-- | A data type with several fields, using record syntax.
data Person = Person
  { name :: String
  , age :: Int
  , address :: String
  } deriving (Data, Show)


p = Person "Alice" 25 "123 Main St"

mt = MyType 1 "Hello"

ms = MySum1 1

ms2 = MySum2 "Hello"

mp = MyProduct 1 ms2

main :: IO ()
main = do
  let p = Person "Alice" 25 "123 Main St"
  let fields = fieldInfo p
  putStrLn "Fields:"
  mapM_ (\(FieldInfo name type_) -> putStrLn $ show name ++ ": " ++ show type_) fields
  print ""
  --print $ typeInfo p
  print $ insertStmtFor p
