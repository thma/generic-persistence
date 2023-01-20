# Writing a Haskell persistence layer using Generics and Reflection

## Introduction

In this article I'll describe how to write a minimalistic Haskell persistence layer (on top of HDBC). 
My approach will rely heavily on Generics (`Data.Data`, `Data.Typeable`) and Reflection (`Type.Reflection`).

The *functional goal* of my persistence layer is to provide hassle-free RDBMS persistence for Haskell data types in 
Record notation (for brevity I call them *Entities*).

That is, it must provide means for inserting, updating, deleting and quering such enties to/from relational databases.

Not in scope for the current state of the library are things like:
- A query language
- User-definable mappings of Haskell types to RDBMS types
- Handling of relationships between entities (1:1, 1:n, n:m)
- Handling of transactions
- Handling of database migrations
- Handling of database schemas
- Handling of database connections and sessions
- Handling of prepared statements
- Handling auto-incrementing primary keys
- Caching
- ...

So as of now it's just about the bare minimum to get some data into a database and to get it back out again by using Generics and Reflection.

The main *design goal* is to minimize the *boilerplate* code required. Ideally I would like to achieve the following:

- no manual instantiation of type classes
- no implementation of encoders/decoders
- no special naming convention for types and their attributes 
- no special types to define entities and attributes
- no Template Haskell scaffolding of glue code

In an ideal world we would be able to take any POHO (Plain old Haskell Object) 
and persist it to any RDBMS without any additional effort.

## Short demo

Here now follows a short demo that shows how the library looks and feels from the user's point of view.

```haskell
{-# LANGUAGE DeriveDataTypeable#-}
module Main (main) where

import Data.Data ( Data )
import TypeInfo ( typeInfo ) 
import GenericPersistence( delete, persist, retrieveAll, retrieveById )
import Database.HDBC (disconnect, runRaw, commit) 
import Database.HDBC.Sqlite3 ( connectSqlite3 )

-- | define a data type with several fields, using record syntax.
data Person = Person
  { personID :: Int
  , name :: String
  , age :: Int
  , address :: String
  } deriving (Data)
```

The persistent data type must be deriving the `Data.Data` type class. This is required
to enable all the Generics magic to work behind the scenes. 
Fortunately, deriving `Data` needs no manual implementation, we get it for free by enabling `DeriveDataTypeable`.

```haskell
main :: IO ()
main = do
    -- initialize Person table
    conn <- connectSqlite3 "sqlite.db"
    runRaw conn "DROP TABLE IF EXISTS Person;"
    runRaw conn "CREATE TABLE IF NOT EXISTS Person (personID INT PRIMARY KEY, name TEXT, age INT, address TEXT);"
    commit conn
```

As of now my library does not cover the creation of database tables. So this is still a manual step.
As already mentioned, the library does not cover any user defined mapping of data type attributes to columns.
As of now the same names for the attributes and the columns are used.
For the column types we are choosing types that can be automatically converted by HDBC. 

Now we move on to using the actual library functions:

```haskell
    -- create a Person entity
    let alice = Person {personID = 123456, name = "Alice", age = 25, address = "Elmstreet 1"}

    -- insert a Person into a database
    persist conn alice

    -- update a Person
    persist conn alice {address = "Main Street 200"}  
    
    -- select a Person from a database
    -- The result type must be provided explicitly, as `retrieveById` has a polymorphic return type `IO a`.
    alice' <- retrieveById conn "123456" :: IO Person

    -- delete a Person from a database
    delete conn alice'
    
    -- close connection
    disconnect conn
```

And here comes the output of the demo program. As you can see, there is some trace output for each of the database operations.

```haskell
ghci> main
Inserting Person 123456 "Alice" 25 "Elmstreet 1"
Updating Person 123456 "Alice" 25 "Main Street 200"
Retrieve Person with id 123456
Deleting Person with id 123456
```

Summarizing, we can state that there is virtually no boilerplate code required in the user code.
The only thing we have to do is to derive the `Data` type class for our persistent data types.
The library takes care of the rest.


## A deeper dive into the library

### The `persist` function

In this section we are taking a closer look at the library internals. Let's start with the `persist` function:

```haskell
-- | A function that persists an entity  to a database.
-- The function takes an HDBC connection and an entity as parameters.
-- The entity is either inserted or updated, depending on whether it already exists in the database.
-- The required SQL statements are generated dynamically using Haskell generics and reflection
persist :: (IConnection conn, Data a) => conn -> a -> IO ()
persist conn entity = do
  resultRows <- quickQuery conn selectStmt []
  case resultRows of
    [] -> do
      trace $ "Inserting " ++ gshow entity
      runRaw conn insertStmt
      commit conn
    [_singleRow] -> do
      trace $ "Updating " ++ gshow entity
      runRaw conn updateStmt
      commit conn
    _ -> error $ "More than one entity found for id " ++ show eid
  where
    ti = typeInfo entity
    eid = entityId entity
    selectStmt = selectStmtFor ti eid
    insertStmt = insertStmtFor entity
    updateStmt = updateStmtFor entity
    
-- | A function that returns the primary key value of an entity as a String.    
entityId :: forall d. (Data d) => d -> String
entityId x = fieldValueAsString x (idColumn (typeInfo x))
```

The overall logic of this function is as follows:

1. Perform a select query against the table corresponding to type `a` to check whether a record is already present for the primary key value derived from `entity`.
2. If the list of resulting rows is empty, the entity has not been persisted before and an `INSERT`-statement has to be excecuted.
3. If the list contains exactly one row, the entity already was stored in the DB and an `UPDATE`-statement has to be executed.
4. If the list contains more than one row, something is wrong and an error is thrown.

The `selectStmtFor`, `insertStmtFor` and `updateStmtFor` functions are used to generate the required SQL statements dynamically.

#### Inserting an entity

Let's start with `insertStmtFor` as it is the simplest one.

Let's say we have a Person entity:

```haskell
alice :: Person
alice = Person {personID = 123456, name = "Alice", age = 25, address = "Elmstreet 1"}
```

Then the corresponding insert statement is:

```sql
INSERT INTO Person (id, name, age, address) VALUES (123456, "Alice", 25, "Elmstreet 1");
```

So in order to generate the insert statement we need to know the table name, the column names and the values.
The idea is to use Haskell Generics to obtain these from the entity instance.
As of now I'm using the type- and attribute-names directly as column names. But this could be easily changed later on.
The tricky business is to dynamically inspect the entity instance and extract the values of the attributes.

So here comes the code for `insertStmtFor`:

```haskell
insertStmtFor :: Data a => a -> String
insertStmtFor x =
  "INSERT INTO "
    ++ typeName x
    ++ " ("
    ++ intercalate ", " (fieldNames x)
    ++ ") VALUES ("
    ++ intercalate ", " (fieldValues x)
    ++ ");"
```

The overall construction of the insert statement is obvious. We just need to know a bit more about the `typeName`, `fieldNames` and `fieldValues` functions from the `TypeInfo` module:

```haskell
-- | A function that returns the (unqualified) type name of an entity.
typeName :: (Data a) => a -> String
typeName = dataTypeName . dataTypeOf
```

The `typeName` function uses the `dataTypeOf :: a -> DataType` function of the `Data`type class to obtain the type of a `Data`instance.

```haskell
-- | A function that take an entity as input paraemeter and returns a list of 
--   Strings representing the values of all fields of the entity.
--   Example: fieldValues (Person "John" 42) = ["John", "42"]
fieldValues :: (Data a) => a -> [String]
fieldValues = gmapQ gshow
```

The function `fieldValues` is a bit more tricky. It uses the `gmapQ` function from the `Data.Data` module to map the `gshow` function over all attributes of the entity. The `gshow` function is a generic version of the `show` function that works on any `Data` instance.
If you want to know more about `gmapQ` and `gshow`, you can read Chris Done's [Typeable and Data in Haskell](https://chrisdone.com/posts/data-typeable/).


To understand the `fieldNames` function, we need to take a look at `TypeInfo` and `FieldInfo` first:

```haskell
-- | A data type that holding information about a type. The Phantom type parameter `a` ensures type safety.
data TypeInfo a = TypeInfo
  { -- | The constructors of the type.
    typeConstructor :: Constr,
    -- | The fields of the type.
    typeFields      :: [FieldInfo]
  }
  deriving (Show)

-- | A data type that holds information about a field of a data type.
data FieldInfo = FieldInfo
  { -- | The name of the field, Nothing if it has none.
    fieldName        :: Maybe String,
    -- | The constructor of the field.
    fieldConstructor :: Constr,
    -- | The type of the field.
    fieldType        :: TypeRep
  }
  deriving (Show)  
```

A `TypeInfo` can be obtained from an entity using the `typeInfo` function:

```haskell
typeInfo :: Data a => a -> TypeInfo a
typeInfo x =
  TypeInfo
    { typeConstructor = toConstr x,
      typeFields = fieldInfo x
    }
```

Where `toConstr :: a -> Constr` is a function from the `Data` type class that returns the constructor of a `Data a` instance; and
`fieldInfo` is a function that returns the list of `FieldInfo` instances for a given entity:

```haskell
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
```

The `fieldInfo` function uses the `constrFields :: Constr -> [String]` function from the `Data` type class to obtain the names of the fields of the constructor of the entity. If type `a` is a record type, the names of the fields are returned. Otherwise an empty list is returned.
This list of candidate field names then used to create a list `names` of `Maybe String` values . If the length of the candidate list is equal to the length of the list of actual field constructors, the entity is a record type and the candidate names are used. Otherwise the list of names is filled with `Nothing` values.

The `gmapQ` function is used to map the `toConstr` and `typeOf` functions over the entity to obtain the constructors and types of the fields.

Finally the `zipWith3` function is used to combine the three lists into a list of `FieldInfo` instances.

Now back to the `fieldNames` function:

```haskell
-- | A function that returns the list of field names of an entity of type `a`.  
fieldNames :: (Data a) => a -> [String]
fieldNames = fieldNamesFromTypeInfo . typeInfo

-- | A function that returns the list of field names of a `TypeInfo a` object.
--   An error is thrown if the type does not have named fields.
fieldNamesFromTypeInfo :: TypeInfo a -> [String]
fieldNamesFromTypeInfo ti = map (expectJust errMsg . fieldName) (typeFields ti)
  where
    errMsg = "Type " ++ tiTypeName ti ++ " does not have named fields"
```

The `fieldNames` function uses the `typeInfo` function to obtain the `TypeInfo` instance for the entity and then maps the `fieldNamesFromTypeInfo` function over all type fields to obtain the list of field names.

This is all tooling that we need to generate the insert statement for an entity by dynamically inspecting its type information. This statement is then used to insert the entity into the database by using the HDBC API.

#### Updating an entity

The update statement is generated in a similar way. The only difference is that we need to know the primary key of the entity in order to generate the `WHERE` clause of the update statement.

```haskell
-- | A function that returns an SQL update statement for an entity. Type 'a' must be an instance of Data.
updateStmtFor :: Data a => a -> String
updateStmtFor x =
  "UPDATE "
    ++ typeName x
    ++ " SET "
    ++ intercalate ", " updatePairs
    ++ " WHERE "
    ++ idColumn ti
    ++ " = "
    ++ fieldValueAsString x (idColumn ti)
    ++ ";"
  where
    updatePairs = zipWith (\n v -> n ++ " = " ++ v) (fieldNames x) (fieldValues x)
    ti = typeInfo x
```

The primary key column is obtained using the `idColumn` function:

```haskell
-- | A function that returns the name of the primary key column for a type 'a'.
--  By convention we are using the following name: convert the type name to lower case and append "ID".
idColumn :: TypeInfo a -> String
idColumn ti = map toLower (tiTypeName ti) ++ "ID"
```

The `fieldValueAsString` function takes an entity and a field name as input parameters and returns the value of the field as a String:

```haskell
-- | A function that takes an entity and a field name as input parameters and returns the value of the field as a String.
--  Example: fieldValueAsString (Person "John" 42) "name" = "John"
--  Example: fieldValueAsString (Person "John" 42) "age" = "42"
--  if the field is not present in the entity, an error is thrown.
fieldValueAsString :: Data a => a -> String -> String
fieldValueAsString x field =
  valueList !! index
  where
    fieldList = fieldNames x
    valueList = fieldValues x
    index =
      expectJust
        ("Field " ++ field ++ " is not present in type " ++ typeName x)
        (elemIndex field fieldList)

-- | A function that take an entity as input parameter and returns a list of 
--   Strings representing the values of all fields of the entity.
--   Example: fieldValues (Person "John" 42) = ["John", "42"]
fieldValues :: (Data a) => a -> [String]
fieldValues = gmapQ gshow
```

#### Selecting an entity

Creating a select query with `selectStmtFor` works slightly different than `insertStmtFor` and `updateStmtFor`. When creating a `SELECT` statement we don't have an entity to inspect. Instead we need to know the type of the entity and the primary key value of the entity we want to select. 
That's why we need to pass the type information and the primary key value as parameters to the `selectStmtFor` function:

```haskell
-- | This function takes a `TypeInfo` object and a primary key value as input parameters and returns a select statement for the entity.
selectStmtFor :: (Show id) => TypeInfo a -> id -> String
selectStmtFor ti eid =
  "SELECT "
    ++ intercalate ", " (fieldNamesFromTypeInfo ti)
    ++ " FROM "
    ++ tiTypeName ti
    ++ " WHERE "
    ++ idColumn ti
    ++ " = "
    ++ show eid
    ++ ";"

-- | A function that returns the (unqualified) type name of `a` from a `TypeInfo a` object.
tiTypeName :: TypeInfo a -> String
tiTypeName = dataTypeName . constrType . typeConstructor    
```

Apart from `tiTypeName` we have already seen all the other ingredients of the `selectStmtFor` function.

### The `retrieveById` function

The `retrieveById` function is the counterpart of the `persist` function. It takes a connection and a primary key value as input parameters and returns the entity with the given primary key value. If no unique entity with the given primary key value exists, an error is thrown.

```haskell
-- | A function that retrieves an entity from a database.
--   The function takes an HDBC connection and an entity id as parameters.
--   It returns the entity of type `a` with the given id.
--   An error is thrown if no such entity exists or if there are more than one entity with the given id.
retrieveById :: forall a conn id. (Data a, IConnection conn, Show id) => conn -> id -> IO a
retrieveById conn eid = do
  let ti = typeInfoFromContext 
      stmt = selectStmtFor ti eid
  trace $ "Retrieve " ++ tiTypeName ti ++ " with id " ++ show eid
  resultRowsSqlValues <- quickQuery conn stmt []
  case resultRowsSqlValues of
    [] -> error $ "No " ++ show (typeConstructor ti) ++ " found for id " ++ show eid
    [singleRowSqlValues] -> do
      return $ 
        expectJust 
          ("No " ++ show (typeConstructor ti) ++ " found for id " ++ show eid) 
          (buildFromRecord ti singleRowSqlValues :: Maybe a)
    _ -> error $ "More than one entity found for id " ++ show eid
```

We have already seen the `selectStmtFor` function in the previous section. But we have two other quite elaborated ingredients to make the `retrieveById` function work:

- The `typeInfoFromContext` function is a helper function that returns the `TypeInfo` instance for the entity type `a`:
- The `buildFromRecord` function takes a `TypeInfo` instance and a list of `SqlValue` objects and returns the entity of type `a` if the list of `SqlValue` objects can be converted to the entity type `a`. If the conversion fails, `Nothing` is returned.

```haskell
-- | This function creates a TypeInfo object from the context of a function call.
--   The Phantom Type `a` is used to convince the compiler that the `TypeInfo a` object really describes type `a`.  
typeInfoFromContext :: forall a . Data a => TypeInfo a
typeInfoFromContext = 
  let dt = dataTypeOf (undefined :: a)   -- looks awkward, but it's the official way...
      constr = head $ dataTypeConstrs dt -- TODO: handle cases with more than one Constructor...
      sample = fromConstr constr :: a
  in typeInfo sample
```

This is quite dense, so let's take it step by step:

First we use ` dataTypeOf :: a -> DataType ` to get the `DataType` object for the type `a`. As we don't have a value of type `a` at hand, we have to take it from thin air. So we use an `undefined :: a` as parameter. This is a bit awkward, but it's the official way to do it. The `undefined` value is never evaluated, so it doesn't matter what value we pass to `dataTypeOf`. But we need to convince the compiler that we really have a value of type `a` at hand.

Then we use `dataTypeConstrs` to get a list of `DataConstr` objects for the type `a`. We take the first `DataConstr` object from this list and use it to create a sample value of type `a` with `fromConstr`. 

Finally this sample value is used to create a `TypeInfo a` object for the type `a`.

Cudos to [the brilliant people on stackoverflow](https://stackoverflow.com/questions/75171829/how-to-obtain-a-data-data-constr-etc-from-a-type-representation/75172846#75172846) for the explanation of how to get the `DataType` object from thin air.

The `buildFromRecord` function is even bit more complex. It takes a `TypeInfo` object and a list of `SqlValue` objects and tries to convert the list of `SqlValue` objects to the entity type `a`. If the conversion fails, `Nothing` is returned:

```haskell
-- | This function takes a `TypeInfo a`and a List of HDBC `SqlValue`s and returns a `Maybe a`.
--  If the construction of an entity fails, Nothing is returned, otherwise Just a.
buildFromRecord :: (Data a) => TypeInfo a -> [SqlValue] -> Maybe a
buildFromRecord ti record = applyConstr ctor dynamicsArgs
  where
    ctor = typeConstructor ti
    types = map fieldType (typeFields ti)
    dynamicsArgs =
      expectJust
        ("buildFromRecord: error in converting record " ++ show record)
        (zipWithM convert types record)
```	

Before we can apply the `applyConstr` function to instantiate an `a` value, we have to do some preparation work. The `[SqlValue]` list contains the values of the fields of the entity as they are coming from the database. In order to use these values to instantiate an `a` value, we have to convert them to the types of the fields of the entity. In order to use them as list elements we wrap them in `Dynamic` objects. 

```haskell
-- | convert a SqlValue into a Dynamic value that is backed by a value of the type represented by the SomeTypeRep parameter.
--  If conversion fails, return Nothing.
--  conversion to Dynamic is required to allow the use of fromDynamic in applyConstr
--  see also https://stackoverflow.com/questions/46992740/how-to-specify-type-of-value-via-typerep
convert :: SomeTypeRep -> SqlValue -> Maybe Dynamic
convert (SomeTypeRep rep) val
  | Just HRefl <- eqTypeRep rep (typeRep @Int) = Just $ toDyn (fromSql val :: Int)
  | Just HRefl <- eqTypeRep rep (typeRep @Double) = Just $ toDyn (fromSql val :: Double)
  | Just HRefl <- eqTypeRep rep (typeRep @String) = Just $ toDyn (fromSql val :: String)
  | otherwise = Nothing
```

The `convert` function takes a `SomeTypeRep`, representing the type of a given entity field, and a `SqlValue` object and tries to convert the `SqlValue` to the type represented by `SomeTypeRep`. If the conversion fails, `Nothing` is returned. This conversion is needed keep the compiler happy. As you can see the list of supported is quite restricted at the moment. But it's easy to extend the list of supported types.

Now we are ready to use the `applyConstr` function to instantiate an `a` value. The `applyConstr` function takes a `Constr` object and a list of `Dynamic` objects and tries to construct an entity of type `a` fromthe list of `Dynamic` objects. If the construction fails, `Nothing` is returned.

```haskell
-- | This function takes a `Constr` and a list of `Dynamic` values and returns a `Maybe a`.
--   If an `a`entity could be constructed, Just a is returned, otherwise Nothing.
--   See also https://stackoverflow.com/questions/47606189/fromconstrb-or-something-other-useful
--   for Info on how to use fromConstrM
applyConstr :: Data a => Constr -> [Dynamic] -> Maybe a
applyConstr ctor args =
  let nextField :: forall d. Data d => StateT [Dynamic] Maybe d
      nextField = StateT uncons >>= lift . fromDynamic
   in case runStateT (fromConstrM nextField ctor) args of
        Just (x, []) -> Just x
        _            -> Nothing -- runtime type error or too few / too many arguments
```

### The `retrieveAll` function

This function is used to retrieve all entities of a given type from the database. It takes an HDBC connection as parameter and returns a list of entities of type `a`. The type `a` is determined by the context of the function call.

As you can see, thisd function reuses most of the ingredients from `retrieveById` but just uses a simpler SQL statement to retrieve all entities of a given type:

```haskell
-- | This function retrieves all entities of type `a` from a database.
--  The function takes an HDBC connection as parameter.
--  The type `a` is determined by the context of the function call.
retrieveAll :: forall a conn. (Data a, IConnection conn) => conn -> IO [a]
retrieveAll conn = do
  let ti = typeInfoFromContext
      stmt = selectAllStmtFor ti
  trace $ "Retrieve all " ++ tiTypeName ti ++ "s"
  resultRowsSqlValues <- quickQuery conn stmt []
  return $ map (expectJust "No entity found") (map (buildFromRecord ti) resultRowsSqlValues :: [Maybe a])

selectAllStmtFor :: TypeInfo a -> String
selectAllStmtFor ti =
  "SELECT "
    ++ intercalate ", " (fieldNamesFromTypeInfo ti)
    ++ " FROM "
    ++ tiTypeName ti
    ++ ";"
```

### The `delete` function

This function is used to delete an entity from the database. It takes an HDBC connection and an entity of type `a` as parameters. It also does not bring anything new to the table. It just uses the `deleteStmtFor` function to generate the SQL statement to delete the entity from the database:
  
```haskell
delete :: (IConnection conn, Data a) => conn -> a -> IO ()
delete conn entity = do
  trace $ "Deleting " ++ typeName entity ++ " with id " ++ entityId entity
  runRaw conn (deleteStmtFor entity)
  commit conn

deleteStmtFor :: Data a => a -> String
deleteStmtFor x =
  "DELETE FROM "
    ++ show (typeName x)
    ++ " WHERE "
    ++ idColumn ti
    ++ " = "
    ++ fieldValueAsString x (idColumn ti)
    ++ ";"
  where
    ti = typeInfo x
```  

## Conclusion

We have learnt ho use `Data` based Generics to implement a simple persistence library. The user of the library will not have to write any boilerplate code. The library will generate the SQL statements and the code to convert the database records to Haskell entities. 

The library is by no means complete. It is just a proof of concept. But it shows that it is possible to use Generics to eliminate a lot of handwritten code.

I'm explicitely asking for your feedback here:
- Do you regard such a persistence API as useful?
- Do you have any suggestions for improvements?
- Which feature would you like to see most urgently?
- Do you think it makes sense to extend this proof of concept to a full fledged solution, 
  or are there already enough libraries out there that do the same?
