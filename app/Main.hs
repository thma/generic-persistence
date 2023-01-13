{-# LANGUAGE DeriveDataTypeable#-}
{-# LANGUAGE ExtendedDefaultRules#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications#-}
{-# LANGUAGE AllowAmbiguousTypes#-}
module Main (main) where

import Data.Data hiding (typeRep, IntRep, TypeRep)

import TypeInfo
import RecordtypeReflection
import SqlGenerator
import Data.Dynamic --(toDyn)
import Type.Reflection
import GHC.Data.Maybe (expectJust)
import Data.Maybe (fromMaybe)
import Control.Monad

-- | A data type with several fields, using record syntax.
data Person = Person
  { id :: !Int
  , name :: !String
  , age :: !Int
  , address :: !String
  } deriving (Data, Show)

p = Person 123456 "Alice" 25 "123 Main St"


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
      strings = ["4712", "Bob Marley", "36", "Tuff Gong Studio, Kingston, Jamaica"]
      
      bobMa = (buildFromRecord (typeInfo p) strings :: Maybe Person)

  print bobMa
  
  entity <- retrieveEntity "4711" (typeInfo p) :: IO (Maybe Person)
  print entity

retrieveEntity :: forall a. (Data a) => String -> TypeInfo -> IO a
retrieveEntity id ti = do
  let 
      stmt = selectStmtFor ti id
      strings = [id, "Robert Zimmermann", "36", "Tuff Gong Studio, Kingston, Jamaica"]
  -- execute stmt  
      bobMa = (buildFromRecord ti strings :: Maybe a)
  return $ fromMaybe (error "No entity found") bobMa

