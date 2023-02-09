--{-# LANGUAGE RankNTypes           #-}
--{-# LANGUAGE ScopedTypeVariables  #-}
module Database.GP.TypeInfo
  ( TypeInfo,
    typeConstructor,
    fieldNames,
    fieldTypes,
    typeName,
    typeInfo,
    typeInfoFromContext,
  )
where

import Data.Data

-- | A data type holding meta-data about a type. 
--   The Phantom type parameter `a` ensures type safety for reflective functions
--   that use this type to create type instances (See module RecordtypeReflection).
data TypeInfo a = TypeInfo
  { typeConstructor :: Constr,
    fieldNames      :: [String],
    fieldTypes      :: [TypeRep]
  }
  deriving (Show)

-- | this function is a smart constructor for TypeInfo objects.
--   It takes a value of type `a` and returns a `TypeInfo a` object.
--   If the type has no named fields, an error is thrown.
--   If the type has more than one constructor, an error is thrown.
typeInfo :: Data a => a -> TypeInfo a
typeInfo x =
  TypeInfo
    { typeConstructor = ensureSingleConstructor (dataTypeOf x),
      fieldNames = fieldNamesOf x,
      fieldTypes = gmapQ typeOf x
    }

-- | This function ensures that the type of `a` has exactly one constructor.
--   If the type has exactly one constructor, the constructor is returned.
--   otherwise, an error is thrown.
ensureSingleConstructor :: DataType -> Constr
ensureSingleConstructor dt =
  case dataTypeConstrs dt of
    [cnstr] -> cnstr
    _ -> error $ "ensureSingleConstructor: Only types with one constructor are supported (" ++ show dt ++ ")"

-- | This function creates a TypeInfo object from the context of a function call.
--   The Phantom Type parameter `a` is used to convince the compiler that the `TypeInfo a` object really describes type `a`.
--   See also https://stackoverflow.com/questions/75171829/how-to-obtain-a-data-data-constr-etc-from-a-type-representation
typeInfoFromContext :: forall a. Data a => TypeInfo a
typeInfoFromContext =
  let dt = dataTypeOf (undefined :: a)    -- This is the trick to get the type a from the context.
      constr = ensureSingleConstructor dt
      evidence = fromConstr constr :: a   -- this is evidence for the compiler that we have a value of type a
   in typeInfo evidence

-- | This function returns the (unqualified) type name of `a` from a `TypeInfo a` object.
typeName :: TypeInfo a -> String
typeName = dataTypeName . constrType . typeConstructor

-- | This function returns the list of field names of an entity of type `a`.
fieldNamesOf :: (Data a) => a -> [String]
fieldNamesOf x = names
  where
    constructor = toConstr x
    candidates = constrFields constructor
    constrs = gmapQ toConstr x
    names =
      if length candidates == length constrs
        then candidates
        else error $ "fieldNamesOf: Type " ++ show (typeOf x) ++ " does not have named fields"
