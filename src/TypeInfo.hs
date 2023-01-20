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
    typeInfoFromContext,
    tiTypeName,
    fieldInfo,
    fieldNames,
    fieldNamesFromTypeInfo
  )
where

import           Data.Data            
import           GHC.Data.Maybe        (expectJust)

{--

https://chrisdone.com/posts/data-typeable/

--}

-- | A data type that holding information about a type. The Phantom type parameter `a` ensures type safety.
data TypeInfo a = TypeInfo
  { -- | The constructors of the type.
    typeConstructor :: Constr,
    -- | The fields of the type.
    typeFields      :: [FieldInfo]
  }
  deriving (Show)

typeInfo :: Data a => a -> TypeInfo a
typeInfo x =
  TypeInfo
    { typeConstructor = toConstr x,
      typeFields = fieldInfo x
    }

-- | This function creates a TypeInfo object from the context of a function call.
--   The Phantom Type `a` is used to convince the compiler that the `TypeInfo a` object really describes type `a`.  
typeInfoFromContext :: forall a . Data a => TypeInfo a
typeInfoFromContext = 
  let dt = dataTypeOf (undefined :: a)   -- This is a trick I learned from https://stackoverflow.com/questions/75171829/how-to-obtain-a-data-data-constr-etc-from-a-type-representation/75172846#75172846
      constr = head $ dataTypeConstrs dt -- TODO: handle cases with more than one Constructor
      sample = fromConstr constr :: a
  in typeInfo sample

-- | A function that returns the (unqualified) type name of an entity.
typeName :: (Data a) => a -> String
typeName = dataTypeName . dataTypeOf

-- | A function that returns the (unqualified) type name of `a` from a `TypeInfo a` object.
tiTypeName :: TypeInfo a -> String
tiTypeName = dataTypeName . constrType . typeConstructor

-- | A data type that holds information about a field of a data type.
data FieldInfo = FieldInfo
  { -- | The name of the field, Nothing if it has none.
    fieldName        :: Maybe String,
    -- | The constr of the field.
    fieldConstructor :: Constr,
    -- | The type of the field.
    fieldType        :: TypeRep
  }
  deriving (Show)

-- | A function that returns a list of FieldInfos representing the name, constructor and type of each field in the data type `a`.
fieldInfo :: (Data a) => a -> [FieldInfo]
fieldInfo x = zipWith3 FieldInfo names constrs types
  where
    constructor = toConstr x
    candidates = constrFields constructor
    constrs = gmapQ toConstr x
    types = gmapQ typeOf x
    names :: [Maybe String] =
      if length candidates == length constrs
        then map Just candidates
        else replicate (length constrs) Nothing

-- | A function that returns the list of field names of an entity of type `a`.  
fieldNames :: (Data a) => a -> [String]
fieldNames = fieldNamesFromTypeInfo . typeInfo

-- | A function that returns the list of field names of a `TypeInfo a` object.
--   An error is thrown if the type does not have named fields.
fieldNamesFromTypeInfo :: TypeInfo a -> [String]
fieldNamesFromTypeInfo ti = map (expectJust errMsg . fieldName) (typeFields ti)
  where
    errMsg = "Type " ++ tiTypeName ti ++ " does not have named fields"


