module Database.GP.Conn
  ( Conn (..),
    connect,
    TxHandling (..),
    ConnectionPool,
    createConnPool,
    withResource,
    quickQuery,
    quickQuery'
  )
where

import           Data.Pool     (Pool, PoolConfig, defaultPoolConfig, newPool,
                                withResource)
import           Database.GP.HdbcCarveout 

-- |
--  This module defines a wrapper around an HDBC IConnection.
--  Using this wrapper `Conn` simplifies the signature of the functions in the `Database.GP` module.
--  It allows to use any HDBC connection without having to define a new function for each connection type.
--  It also provides additional attributes to the connection, like the database type and the implicit commit flag.
--  These attributes can be used to implement database specific functionality, modify transaction behaviour, etc.
--
--  This code has been inspired by the HDBC ConnectionWrapper and some parts have been copied verbatim
--  from the HDBC Database.HDBC.Types module.
--
--  This module also defines a ConnectionPool type, which provides basic connection pooling functionality.

-- | A wrapper around an HDBC IConnection.
data Conn = forall conn. IConnection conn => 
  Conn 
    Bool -- | If True, the GenericPersistence functions will commit the transaction after each operation.
    conn -- | The wrapped HDBC IConnection

data TxHandling = AutoCommit | ExplicitCommit

-- | a smart constructor for the Conn type.
connect :: forall conn. IConnection conn => TxHandling -> conn -> Conn
connect = \case
  AutoCommit     -> Conn True
  ExplicitCommit -> Conn False

-- | allows to execute a function that requires an `IConnection` argument on a `Conn`.
withWConn :: forall b. Conn -> (forall conn. IConnection conn => conn -> b) -> b
withWConn (Conn _ic conn) f = f conn

-- | manually implement the IConnection type class for the Conn type.
instance IConnection Conn where
  disconnect w = withWConn w disconnect
  commit w = withWConn w commit
  rollback w = withWConn w rollback
  runRaw w = withWConn w runRaw
  run w = withWConn w run
  prepare w = withWConn w prepare

quickQuery :: Conn -> String -> [SqlValue] -> IO [[SqlValue]]
quickQuery conn sql vals = do
  stmt <- prepare conn sql
  execute stmt vals
  fetchAllRows stmt

quickQuery' :: Conn -> String -> [SqlValue] -> IO ()
quickQuery' conn sql vals = do
  stmt <- prepare conn sql
  execute stmt vals
  return ()

fetchAllRows :: Statement -> IO [[SqlValue]]
fetchAllRows stmt = do
  row <- fetchRow stmt
  case row of
    Nothing -> pure []
    Just r -> (r :) <$> fetchAllRows stmt

-- | A pool of connections.
type ConnectionPool = Pool Conn

-- | Creates a connection pool.
createConnPool ::
  IConnection conn =>
  -- | the transaction mode
  TxHandling ->
  -- | the connection string
  String ->
  -- | a function that takes a connection string and returns an IConnection
  (String -> IO conn) ->
  -- | the time (in seconds) to keep idle connections open
  Double ->
  -- | the maximum number of connections to keep open
  Int ->
  -- | the resulting connection pool
  IO ConnectionPool
createConnPool txHandling connectString connectFun idle numConns = newPool poolConfig
  where
    freshConnection :: IO Conn
    freshConnection = connect txHandling <$> connectFun connectString
    poolConfig :: PoolConfig Conn
    poolConfig = defaultPoolConfig freshConnection disconnect idle numConns
