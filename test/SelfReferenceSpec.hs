{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NamedFieldPuns #-}

module SelfReferenceSpec
--  ( test,
--    spec,
--  )
where

import Database.GP.GenericPersistence
import Database.GP.Entity 
import Database.HDBC ( fromSql, toSql, SqlValue )
import Database.HDBC.Sqlite3 ( connectSqlite3 )
import GHC.Generics ( Generic )
import Test.Hspec ( hspec, describe, it, shouldBe, Spec )
import Data.Foldable ( forM_ )
import Data.Text ( Text )
import GHC.Base (IO(IO))
import           Data.Typeable
import GHC.Generics


-- `test` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
-- (start up stack repl --test to bring up ghci and have access to all the test functions)
test :: IO ()
test = hspec spec

prepareDB :: IO Conn
prepareDB = do
  conn <- connect AutoCommit <$> connectSqlite3 ":memory:"
  -- create the table: CREATE TABLE Employee (name TEXT PRIMARY KEY, age INTEGER, bossName TEXT
  setupTable @Employee conn defaultSqliteMapping
  return conn


data Employee = Employee
  { name :: Text
  , age :: Int
  , boss :: Maybe Employee
  } deriving (Generic, Show, Eq)

{--
oneToOneFromRow :: forall a b. (Entity a, Entity b, (GFromRow (Rep a))) => String -> (a -> b) -> Conn -> [SqlValue] -> IO a
oneToOneFromRow fieldName getter conn row = do
  let columnIndex = fieldIndex @a fieldName
  maybeReferenced <- selectById conn (row !! columnIndex) :: IO (Maybe b)
  let ti = typeInfo @a
  
  let raw = to @a (gfromRow row)
  return undefined
  --let complete = raw {getter = maybeReferenced}

  --return complete

  -- return $ boss (fromRow conn row) maybeBoss
  --where
  --  col i = fromSql (row !! i)
-}
  


instance Entity Employee Text where
  autoIncrement = False 
  idField = "name"                      -- the name field is the primary key

  -- fieldsToColumns :: [(String, String)] -- omitting the "boss" field,
  -- fieldsToColumns =                     -- as this can not be mapped to a single column
  --   [ ("name", "name"),                 -- instead we use a new column bossName to store the primary key of the boss
  --     ("age", "age"),
  --     ("bossName", "bossName")
  --   ]

  -- this functions defines how to create an Employee from a row in the database
  -- for the fields 'name' and 'age' it is straightforward: just use fromSql to convert the SqlValue to the desired type of the field
  -- for the 'boss' field it is a bit more complicated:
  -- it loads the boss by the foreign key and merges it to the raw employee record
  fromRow :: Conn -> [SqlValue] -> IO Employee
  fromRow conn row = do
    let fkValue = fromSql (row !! 2) :: Text
    maybeBoss <- selectById @Employee conn fkValue  -- load boss by foreign key
    return $ rawEmployee {boss = maybeBoss}         -- merge the boss to the employee
    where
      rawEmployee =
        Employee 
          (col 0)
          (col 1)
          Nothing -- no Boss in the raw employee
        where
          col i = fromSql (row !! i)

  -- this function defines how to take an Employee and map it to a row of SqlValues to be inserted into the database
  -- for the fields 'name' and 'age' it is straightforward: just use toSql to convert the field to a SqlValue
  -- for the 'boss' field it is a bit more complicated:
  -- it first persists the boss to the database and then converts the primary key of the boss to a SqlValue. )
  -- if no boss is present, it just returns an empty Text
  toRow :: Conn -> Employee -> IO [SqlValue]
  toRow conn emp = do
    forM_ (boss emp) (upsert conn) -- persist the boss first.
    return
      [ toSql (name emp),
        toSql (age emp),
        toSql $ maybe "" name (boss emp)
      ]


-- for the test we define some employees
-- Alice is the boss of Bob, who is the boss of Charlie
charlie :: Employee
charlie =
  Employee
    { name = "Charlie",
      age = 33,
      boss = Just bob
    }

bob :: Employee
bob =
  Employee
    { name = "Bob",
      age = 44,
      boss = Just alice
    }

alice :: Employee
alice =
  Employee
    { name = "Alice",
      age = 22,
      boss = Nothing
    }


-- here we are testing that if we 
spec :: Spec
spec = do
  describe "Handling of 1:1 References on same table" $ do
    it "works like a charm" $ do
      -- given, we have a DB and insert charlie
      conn <- prepareDB
      upsert conn charlie

      -- when we select for "Charlie"
      employee' <- selectById conn ("Charlie" :: Text) :: IO (Maybe Employee)
      employee' `shouldBe` Just charlie

      -- then we should find charlie with his boss bob. And bob with his boss alice
      case employee' of
        Nothing -> fail "no employee Charlie found"
        Just charlie' -> do
          let boss' = boss charlie'
          boss' `shouldBe` Just bob
          case boss charlie' of
            Nothing -> fail "no boss found for Charlie"
            Just bob' -> do
              let boss'' = boss bob'
              boss'' `shouldBe` Just alice
            

      
