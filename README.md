# Writing a Haskell persistence layer using Generics and Reflection

## Introduction

In this article I'll describe how to write a minimalistic Haskell persistence layer (on top of HDBC). 
My approach will rely heavily on Generics (`Data.Data`, `Data.Typeable`) and Reflection (`Type.Reflection`).

<!--
Some twenty years back the Java community became increasingly unhappy with the persistence mechanism provided by SUN, 
the Entity Beans of the Enterprise Java Beans (EJB) framework. The EJB framework required developers to implement 
complex interfaces and to write a lot of boilerplate code to integrate into the heavy machinery of the EJB-container.

Developers wanted to have persistence features for their [plain old Java objects* 
(POJOs)](https://en.wikipedia.org/wiki/Plain_old_Java_object) without all the boilerplate and dependencies on awkward frameworks.
-->

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
- Handling auto-incrementing primary keys
- Caching
- ...

So as of now it's just about the bare minimum to get some data into a database and to get it back out again.

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

I'm explicitely asking for your feedback here:
- Do you regard such a persistence API as useful?
- Do you have any suggestions for improvements?
- Do you think it makes sense to continue working on it, or are there already enough libraries out there that do the same?

## A deeper dive into the library

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
