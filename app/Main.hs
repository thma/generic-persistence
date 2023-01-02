{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Data

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

data FieldInfo = FieldInfo
  { fieldName :: Maybe String -- ^ The name of the field, if it has one.
  , fieldType :: TypeRep      -- ^ The type of the field.
  } deriving (Show)
    
data TypeInfo = TypeInfo
  { typeName :: DataType      -- ^ The name of the type.
  , typeConstrs :: [Constr]   -- ^ The constructors of the type.
  , typeFields :: [FieldInfo] -- ^ The fields of the type.
  } deriving (Show)
  
typeInfo :: Data a => a -> TypeInfo  
typeInfo x = TypeInfo
  { typeName = dataTypeOf x
  , typeConstrs = dataTypeConstrs $ dataTypeOf x
  , typeFields = fieldInfo x
  } 
    
-- | A function that returns a list of strings representing the name and type of each field in a data type.
fieldInfo :: (Data a) => a -> [FieldInfo]
fieldInfo x = zipWith FieldInfo names types
  where
    constructor = head $ dataTypeConstrs $ dataTypeOf x
    candidates = constrFields constructor
    types = gmapQ typeOf x
    names = if length candidates == length types
            then map Just candidates
            else replicate (length types) Nothing



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
  print $ typeInfo p
