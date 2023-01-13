{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications#-}
module RecordtypeReflection 
  (
    buildFromRecord
  , applyConstr
  , convert
  )
where

import Data.Data hiding (typeRep, IntRep)
import TypeInfo
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)
import Type.Reflection (SomeTypeRep(..), typeRep, eqTypeRep)
import GHC.Data.Maybe (expectJust)
import Control.Monad (zipWithM)

  
buildFromRecord :: (Data a) => TypeInfo -> [String] -> Maybe a
buildFromRecord ti record = applyConstr ctor dynamicsArgs 
  where
    ctor = typeConstructor ti
    types = map fieldType (typeFields ti) 
    dynamicsArgs = expectJust 
      ("buildFromRecord: error in converting record " ++ show record) 
      (zipWithM convert types record)  

-- https://stackoverflow.com/questions/47606189/fromconstrb-or-something-other-useful
applyConstr :: Data a => Constr -> [Dynamic] -> Maybe a
applyConstr ctor args = let
   nextField :: forall d. Data d => StateT [Dynamic] Maybe d
   nextField = do
      as <- get
      case as of
         [] -> lift Nothing  -- too few arguments
         (a:rest) -> do
            put rest
            case fromDynamic a of
               Nothing -> lift Nothing  -- runtime type mismatch
               Just x  -> return x
   in case runStateT (fromConstrM nextField ctor) args of
      Just (x, []) -> Just x
      _            -> Nothing  -- runtime type error or too few / too many arguments


-- https://stackoverflow.com/questions/46992740/how-to-specify-type-of-value-via-typerep
-- | Parse a string into a value of the type represented by the SomeTypeRep parameter.
--  If parsing fails, return Nothing.
--  If parsing succeeds, return Just the value.
convert :: SomeTypeRep -> String -> Maybe Dynamic
convert (SomeTypeRep rep) str
  | Just HRefl <- eqTypeRep rep (typeRep @Int)    = Just $ toDyn (read str :: Int)
  | Just HRefl <- eqTypeRep rep (typeRep @Double) = Just $ toDyn (read str :: Double)
  | Just HRefl <- eqTypeRep rep (typeRep @String) = Just $ toDyn str
  | otherwise = Nothing
  
  
