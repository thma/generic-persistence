module SqlGenerator 
  ( insertStmtFor
  ) where


import Data.Data (Data, gmapQ)
import Data.Maybe
import Data.List (intercalate, elemIndex)
import TypeInfo
import Data.Generics.Aliases (mkT)
  
insertStmtFor :: Data a => a -> String
insertStmtFor x = "INSERT INTO " 
  ++ show (typeName $ typeInfo x) 
  ++ " (" ++ intercalate ", " fieldList
  ++ ") VALUES (" 
  ++ intercalate ", " valueList
  ++ ");" where 
    fieldList = map (fromJust . fieldName) $ fieldInfo x
    valueList = map (getValueAsString x) fieldList 
 
-- | A function that returns a string representation of the value of a field in a data type.
--  The field is specified by name.
-- The value is returned as a string that can be used in an SQL statement.
-- For example, if the field is an Int, the string will be the string representation of the Int. 
-- The problem here is to reify the field name into a value of type FieldInfo.
-- This is done by using the Data.Data function gmapQ to get a list of all the fields in the data type.

getValueAsString :: Data a => a -> String -> String 
getValueAsString x fieldName = fieldName
--  show $ fromJust $ gmapQ (const Nothing) x !! index where
--  index = fromJust $ elemIndex fieldName fieldList
--  fieldList = _t -- map (fromJust . fieldName) $ fieldInfo x


--transformExample :: (Data a, Data b) => (Int -> Int) -> a -> [a]
--transformExample f = gmapQ (mkT f)

  
{-- 

-- Define a function that takes a field name, a record, and a typeable value,
-- and returns the value of the field
getField :: forall a b. (Typeable a, Typeable b) => String -> a -> Maybe b
getField field record =
  let fieldVal = fromMaybe (error "field not found") (Data.Typeable.cast fieldVal')
  in fieldVal
  where fieldVal' = getField' field record

-- Helper function that gets the field value using reflection
getField' :: forall a. Typeable a => String -> a -> Maybe a
getField' field record = do
  recordType <- typeRepTyCon (typeOf record)
  fieldName <- find (\(name, _, _) -> name == field) (typeRepFields (typeOf record))
  return (fieldVal record)
  where
    fieldVal = fromJust (Data.Typeable.cast fieldVal')
    fieldVal' = getField'' field record

-- Helper function that gets the field value using field labels
getField'' :: forall a. Typeable a => String -> a -> a
getField'' field record = fromJust (Data.Typeable.cast (fieldLabel field record))
--}
