module TypeInfo
  ( TypeInfo (..),
    typeName,
    typeInfo,
    typeInfoFromContext,
  )
where

import           Data.Data            

-- | A data type that holding information about a type. The Phantom type parameter `a` ensures type safety.
data TypeInfo a = TypeInfo
  { -- | The constructors of the type.
    typeConstructor :: Constr,
    fieldNames      :: [String],
    fieldTypes      :: [TypeRep]
  }
  deriving (Show)

typeInfo :: Data a => a -> TypeInfo a
typeInfo x =
  TypeInfo
    { typeConstructor = toConstr x,
      fieldNames = fieldNamesOf x,
      fieldTypes = gmapQ typeOf x      
    }

-- | This function creates a TypeInfo object from the context of a function call.
--   The Phantom Type parameter `a` is used to convince the compiler that the `TypeInfo a` object really describes type `a`.  
--   See also https://stackoverflow.com/questions/75171829/how-to-obtain-a-data-data-constr-etc-from-a-type-representation
typeInfoFromContext :: forall a . Data a => TypeInfo a
typeInfoFromContext = 
  let dt = dataTypeOf (undefined :: a)   -- This is the trick to get the type from the context. 
      constr = case dataTypeConstrs dt of
        [cnstr] -> cnstr
        _       -> error "typeInfoFromContext: Only types with one constructor are supported"
      evidence = fromConstr constr :: a  -- this is evidence for the compiler that we have a value of type a
  in typeInfo evidence

-- | A function that returns the (unqualified) type name of `a` from a `TypeInfo a` object.
typeName :: TypeInfo a -> String
typeName = dataTypeName . constrType . typeConstructor

-- | A function that returns the list of field names of an entity of type `a`.  
fieldNamesOf :: (Data a) => a -> [String]
fieldNamesOf x = names
  where
    constructor = toConstr x
    candidates = constrFields constructor
    constrs = gmapQ toConstr x
    names = if length candidates == length constrs
      then candidates
      else error "fieldNamesOf: Type does not have named fields"
