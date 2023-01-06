module TypeInfo
  ( 
    TypeInfo(..) 
  , FieldInfo(..)
  , typeInfo
  , fieldInfo
  ) where
    
import Data.Data
import Data.Maybe
import Data.List
    
data FieldInfo = FieldInfo
  { fieldName :: Maybe String -- ^ The name of the field, Nothing if it has none.
  , fieldType :: TypeRep      -- ^ The type of the field.
  } deriving (Show)
    
data TypeInfo = TypeInfo
  { typeName :: TypeRep      -- ^ The name of the type.
  , typeConstrs :: [Constr]   -- ^ The constructors of the type.
  , typeFields :: [FieldInfo] -- ^ The fields of the type.
  } deriving (Show)
  
typeInfo :: Data a => a -> TypeInfo  
typeInfo x = TypeInfo
  { typeName = typeOf x
  , typeConstrs = dataTypeConstrs $ dataTypeOf x
  , typeFields = fieldInfo x
  } 
  

    
-- | A function that returns a list of FieldInfos representing the name and type of each field in a data type.
fieldInfo :: (Data a) => a -> [FieldInfo]
fieldInfo x = zipWith FieldInfo names types
  where
    constructor = head $ dataTypeConstrs $ dataTypeOf x
    candidates = constrFields constructor
    types = gmapQ typeOf x
    names = if length candidates == length types
              then map Just candidates
              else replicate (length types) Nothing

    


