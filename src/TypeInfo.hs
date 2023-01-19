{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module TypeInfo
  ( TypeInfo (..),
    FieldInfo (..),
    typeName,
    typeInfo,
    typeInfoFrom,
    tiTypeName,
    fieldInfo,
    fieldNames,
    fieldNamesFromTypeInfo,
    fieldValues,
    gshow,
  )
where

import           Data.Data             (Constr, Data (gmapQ, toConstr), TypeRep,
                                        constrFields, showConstr, typeOf, DataType, dataTypeConstrs, dataTypeRep, fromConstr)
import           Data.Generics.Aliases (extQ)
import           GHC.Data.Maybe        (expectJust)

{--

https://chrisdone.com/posts/data-typeable/

--}

data TypeInfo = TypeInfo
  { -- | The constructors of the type.
    typeConstructor :: Constr,
    -- | The fields of the type.
    typeFields      :: [FieldInfo]
  }
  deriving (Show)

typeInfo :: Data a => a -> TypeInfo
typeInfo x =
  TypeInfo
    { typeConstructor = toConstr x,
      typeFields = fieldInfo x
    }

typeInfoFrom :: forall a. Data a => DataType -> TypeInfo
typeInfoFrom dt =
  TypeInfo
    { typeConstructor = constr, 
      typeFields = fieldInfo sample
    }
    where
      constr = head $ dataTypeConstrs dt
      sample = fromConstr constr :: a

typeName :: (Data a) => a -> String
typeName = show . toConstr

tiTypeName :: TypeInfo -> String
tiTypeName = show . typeConstructor

data FieldInfo = FieldInfo
  { -- | The name of the field, Nothing if it has none.
    fieldName        :: Maybe String,
    -- | The constr of the field.
    fieldConstructor :: Constr,
    -- | The type of the field.
    fieldType        :: TypeRep
  }
  deriving (Show)

-- | A function that returns a list of FieldInfos representing the name, constructor and type of each field in a data type.
fieldInfo :: (Data a) => a -> [FieldInfo]
fieldInfo x = zipWith3 FieldInfo names constrs types
  where
    constructor = toConstr x
    candidates = constrFields constructor
    constrs = gmapQ toConstr x
    types = gmapQ typeOf x
    names =
      if length candidates == length constrs
        then map Just candidates
        else replicate (length constrs) Nothing
   
fieldNames :: (Data a) => a -> [String]
fieldNames x = fieldNamesFromTypeInfo $ typeInfo x

fieldValues :: (Data a) => a -> [String]
fieldValues = gmapQ gshow

fieldNamesFromTypeInfo :: TypeInfo -> [String]
fieldNamesFromTypeInfo ti = map (expectJust errMsg . fieldName) (typeFields ti)
  where
    errMsg = "Type " ++ show (typeConstructor ti) ++ " does not have named fields"

-- | Generic show: taken from syb package / https://chrisdone.com/posts/data-typeable/
gshow :: Data a => a -> String
gshow x = gshows x ""

gshows :: Data a => a -> ShowS
gshows = render `extQ` (shows :: String -> ShowS)
  where
    render t
      | isTuple =
          showChar '('
            . drop 1
            . commaSlots
            . showChar ')'
      | isNull = showString "[]"
      | isList =
          showChar '['
            . drop 1
            . listSlots
            . showChar ']'
      | otherwise =
          constructor
            . slots
      where
        constructor = showString . showConstr . toConstr $ t
        slots = foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t
        commaSlots = foldr (.) id . gmapQ ((showChar ',' .) . gshows) $ t
        listSlots = foldr (.) id . init . gmapQ ((showChar ',' .) . gshows) $ t
        isTuple = all (== ',') (filter (not . flip elem "()") (constructor ""))
        isNull = all (`elem` "[]") (constructor "")
        isList = constructor "" == "(:)"
