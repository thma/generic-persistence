{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module TypeInfo
  ( TypeInfo (..),
    FieldInfo (..),
    typeInfo,
    fieldInfo,
    fieldValueAsString,
    fieldNames,
    fieldNamesFromTypeInfo,
    fieldValues,
    gshow,
  )
where

import           Data.Data             hiding (IntRep, typeRep)
import           Data.Generics.Aliases (extQ)
import           Data.List             (elemIndex)
import           Data.Maybe            (fromMaybe)
import           GHC.Data.Maybe        (expectJust)

{--

https://chrisdone.com/posts/data-typeable/

--}

data TypeInfo = TypeInfo
  { -- | The name of the type.
    typeName        :: TypeRep,
    -- | The constructors of the type.
    typeConstructor :: Constr,
    -- | The fields of the type.
    typeFields      :: [FieldInfo]
  }
  deriving (Show)

typeInfo :: Data a => a -> TypeInfo
typeInfo x =
  TypeInfo
    { typeName = typeOf x,
      typeConstructor = toConstr x,
      typeFields = fieldInfo x
    }

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
fieldNamesFromTypeInfo ti = map (fromMaybe (error errMsg) . fieldName) (typeFields ti)
  where
    errMsg = "Type " ++ show (typeName ti) ++ " does not have named fields"

fieldValueAsString :: Data a => a -> String -> String
fieldValueAsString x field =
  valueList !! index
  where
    fieldList = fieldNames x
    valueList = fieldValues x
    index =
      expectJust
        ("Field " ++ field ++ " is not present in type " ++ show (typeName $ typeInfo x))
        (elemIndex field fieldList)

-- | Generic show: taken from syb package
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
      | otherwise -- showChar '('
        =
          constructor
            . slots
      where
        -- . showChar ')'

        constructor = showString . showConstr . toConstr $ t
        slots = foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t
        commaSlots = foldr (.) id . gmapQ ((showChar ',' .) . gshows) $ t
        listSlots = foldr (.) id . init . gmapQ ((showChar ',' .) . gshows) $ t
        isTuple = all (== ',') (filter (not . flip elem "()") (constructor ""))
        isNull = all (`elem` "[]") (constructor "")
        isList = constructor "" == "(:)"
