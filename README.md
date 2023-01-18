# Writing a Haskell persistence layer using Generics and Reflection

## Introduction

In this article I'll describe how to write a rudimentary Haskell persistence layer (on top of HDBC). 
My approach will rely heavily on Generics (`Data.Data`, `Data.Typeable`) and Reflection (`Type.Reflection`).

<!--
Some twenty years back the Java community became increasingly unhappy with the persistence mechanism provided by SUN, 
the Entity Beans of the Enterprise Java Beans (EJB) framework. The EJB framework required developers to implement 
complex interfaces and to write a lot of boilerplate code to integrate into the heavy machinery of the EJB-container.

Developers wanted to have persistence features for their [plain old Java objects* 
(POJOs)](https://en.wikipedia.org/wiki/Plain_old_Java_object) without all the boilerplate and dependencies on awkward frameworks.
-->


The *functional goal* of my persistence layer is to provide hassle-free RDBMS Persistence for Haskell data types in 
record notation (for brevity I call them *Entities*). 
That is, it must provide means for inserting, updating, deleting and quering such enties to/from relational databases.

Not in scope for the current state of the library are things like:
- A query language
- User-definable mappings of Haskell types to RDBMS types
- Handling of relationships between entities (1:1, 1:n, n:m)
- Handling of transactions
- Handling of database migrations
- Handling of database schemas
- Handling of database connections and sessions
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

Here comes a short demo that demonstrate how close my library comes to the proclaimed design goals.

```haskell
{-# LANGUAGE DeriveDataTypeable#-}
module Main (main) where

import Data.Data ( Data )
import TypeInfo ( typeInfo ) 
import GenericPersistence( deleteEntity, persistEntity, retrieveAllEntities, retrieveEntityById )
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
to enable all the Generics magics to work behind the scenes. 
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

As of now my library does not cover the creation of database tables. So we have to do it manually.
As already mentioned, the library does not cover any user defined mapping of data type attributes to columns.
As of now the same names for the attributes and the columns are used.
For the column types we are choosing types that can be automatically converted by HDBC.

```haskell
    -- create a Person entity
    let alice = Person {personID = 123456, name = "Alice", age = 25, address = "Elmstreet 1"}

    -- insert a Person into a database
    persistEntity conn alice

    -- update a Person
    persistEntity conn alice {address = "Main Street 200"}  
```

So far everything works as advertised. I can insert and update entities.
In the next section we'll see a minor deviation:

```haskell
    -- select a Person from a database
    alice' <- retrieveEntityById conn (typeInfo p) 123456 :: IO Person
```

The `retrieveEntityById` function requires an additional argument, the `TypeInfo` of the entity type.
This is required to enable the library to find the correct table and columns for the entity type.

As of now I did not find a way to get the `TypeInfo` of a type dynamically via a `TypeRep` 
that might be obtained at runtime by a call like `typeRep ([] :: [a])` within the `retrieveEntityById` function.

I assume that this should be easy to fix and is not a major drawback.

The final part of the demo then again works as expected:

```haskell
    -- delete a Person from a database
    deleteEntity conn alice 

    -- close connection
    disconnect conn
```

ANd here comes the output of the demo program:

```haskell
ghci> main
Inserting Person 123456 "Alice" 25 "Elmstreet 1"
Updating Person 123456 "Alice" 25 "Main Street 200"
Retrieve Person with id 123456
Retrieved from DB: Person 123456 "Alice" 25 "Main Street 200"
Deleting Person with id 123456
```

Summarizing, we can state that most of my design goals are met. 
I'm eager to learn if you would regard such a persistence API as useful and if you have any suggestions for improvements!

## A deeper dive into the library

In this section we are taking a closer look at the library internals. Let's start with the `persistEntity` function:

```haskell
-- | A function that persists an entity  to a database.
-- The function takes an HDBC connection and an entity (fulfilling constraint 'Data a') as parameters.
-- The entity is either inserted or updated, depending on whether it already exists in the database.
-- The required SQL statements are generated dynamically using Haskell generics and reflection
persistEntity :: (IConnection conn, Data a) => conn -> a -> IO ()
persistEntity conn entity = do
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

entityId :: forall d. (Data d) => d -> String
entityId x = fieldValueAsString x (idColumn (typeInfo x))

trace :: String -> IO ()
trace = putStrLn
```

The overall logic in this function is as follows:

1. Perform a select query against the table corresponding to type `a` to check whether a record is already present for the primary key value derived from `entity`.
2. If the list of resulting rows is empty, the entity has not been persisted before and an `INSERT`-statement has to be excecuted.
3. If the list contains exactly one row, the entity already was stored in the DB and an `UPDATE`-statement has to be executed.
4. If the list contains more than one row, something is wrong and an error is thrown.

The `selectStmt`, `insertStmt` and `updateStmt` are generated dynamically using the `selectStmtFor`, `insertStmtFor` and `updateStmtFor` functions.

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
import           TypeInfo             (TypeInfo, fieldNames,
                                       fieldNamesFromTypeInfo, fieldValues,
                                       typeInfo, typeName)

insertStmtFor :: Data a => a -> String
insertStmtFor x =
  "INSERT INTO "
    ++ show (typeName $ typeInfo x)
    ++ " ("
    ++ intercalate ", " (fieldNames x)
    ++ ") VALUES ("
    ++ intercalate ", " (fieldValues x)
    ++ ");"
```

