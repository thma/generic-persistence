{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module GenericPersistence
  ( retrieveById,
    retrieveAll,
    persist,
    delete,
  )
where

import           Data.Data
import           Database.HDBC        (IConnection, commit, quickQuery, runRaw)
import           GHC.Data.Maybe       (expectJust)
import           RecordtypeReflection (buildFromRecord, fieldValueAsString)
import           SqlGenerator         (deleteStmtFor, idColumn, insertStmtFor,
                                       selectAllStmtFor, selectStmtFor,
                                       updateStmtFor)
import           TypeInfo             (TypeInfo(..), gshow, typeInfo, typeName)

{--
 This module defines RDBMS Persistence operations for Record Data Types that are instances of 'Data'.
 I call instances of such a data type Entities.

 The Persistence operations are using Haskell generics to provide compile time reflection capabilities.
 HDBC is used to access the RDBMS.
--}

-- | A function that retrieves an entity from a database.
-- The function takes an HDBC connection and an entity id as parameters.
-- It returns the entity of type `a` with the given id.
-- An error is thrown if no entity with the given id exists or if more than one entity with the given id exist.
retrieveById :: forall a conn id. (Data a, IConnection conn, Show id) => conn -> id -> IO a
retrieveById conn eid = do
  let (ti,_) = computeTypeInfo :: (TypeInfo, a) -- returning an `a` convinces the compiler that the TypeInfo represents type `a`
      stmt = selectStmtFor ti eid
  trace $ "Retrieve " ++ show (typeConstructor ti) ++ " with id " ++ show eid
  resultRowsSqlValues <- quickQuery conn stmt []
  case resultRowsSqlValues of
    [] -> error $ "No " ++ show (typeConstructor ti) ++ " found for id " ++ show eid
    [singleRowSqlValues] -> do
      return $ expectJust ("No " ++ show (typeConstructor ti) ++ " found for id " ++ show eid) (buildFromRecord ti singleRowSqlValues :: Maybe a)
    _ -> error $ "More than one entity found for id " ++ show eid
  
computeTypeInfo :: forall a . Data a => (TypeInfo, a)
computeTypeInfo = 
  let dt = dataTypeOf (undefined :: a) 
      constr = head $ dataTypeConstrs dt
      sample = fromConstr constr :: a
  in (typeInfo sample, sample)


retrieveAll :: forall a conn. (Data a, IConnection conn) => conn -> IO [a]
retrieveAll conn = do
  let (ti,_) = computeTypeInfo :: (TypeInfo, a) -- returning an `a` convinces the compiler that the TypeInfo represents type `a`
      stmt = selectAllStmtFor ti
  trace $ "Retrieve all " ++ show (typeConstructor ti) ++ "s"
  resultRowsSqlValues <- quickQuery conn stmt []
  return $ map (expectJust "No entity found") (map (buildFromRecord ti) resultRowsSqlValues :: [Maybe a])



-- | A function that persists an entity  to a database.
-- The function takes an HDBC connection and an entity (fulfilling constraint 'Data a') as parameters.
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
    
entityId :: forall d. (Data d) => d -> String
entityId x = fieldValueAsString x (idColumn (typeInfo x))
    

delete :: (IConnection conn, Data a) => conn -> a -> IO ()
delete conn entity = do
  trace $ "Deleting " ++ typeName entity ++ " with id " ++ entityId entity
  runRaw conn (deleteStmtFor entity)
  commit conn

trace :: String -> IO ()
trace = putStrLn