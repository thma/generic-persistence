module Database.GP.Conn
  ( Conn (..),
    connect,
    Database (..),
    ConnectionPool,
    createConnPool,
    withResource,
  )
where

import           Control.Monad ((>=>))
import           Data.Pool     (Pool, PoolConfig, defaultPoolConfig, newPool,
                                withResource)
import           Database.HDBC (IConnection (..))

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
data Conn = forall conn.
  IConnection conn =>
  Conn
  { -- | The database type
    db             :: Database,
    -- | If True, the GenericPersistence functions will commit the transaction after each operation.
    implicitCommit :: Bool,
    -- | The wrapped connection
    connection     :: conn
  }

-- | An enumeration of the supported database types.
data Database = Postgres | MySQL | SQLite | Oracle | MSSQL
  deriving (Show, Eq, Enum)

-- | a smart constructor for the Conn type.
connect :: forall conn. IConnection conn => Database -> conn -> Conn
connect db = Conn db True

-- | allows to execute a function that requires an `IConnection` argument on a `Conn`.
withWConn :: forall b. Conn -> (forall conn. IConnection conn => conn -> b) -> b
withWConn (Conn _db _ic conn) f = f conn

-- | manually implement the IConnection type class for the Conn type.
instance IConnection Conn where
  disconnect w = withWConn w disconnect
  commit w = withWConn w commit
  rollback w = withWConn w rollback
  runRaw w = withWConn w runRaw
  run w = withWConn w run
  prepare w = withWConn w prepare
  clone w@(Conn db ic _) = withWConn w (clone >=> return . Conn db ic)
  hdbcDriverName w = withWConn w hdbcDriverName
  hdbcClientVer w = withWConn w hdbcClientVer
  proxiedClientName w = withWConn w proxiedClientName
  proxiedClientVer w = withWConn w proxiedClientVer
  dbServerVer w = withWConn w dbServerVer
  dbTransactionSupport w = withWConn w dbTransactionSupport
  getTables w = withWConn w getTables
  describeTable w = withWConn w describeTable

-- | A pool of connections.
type ConnectionPool = Pool Conn

-- | Creates a connection pool.
createConnPool :: IConnection conn =>
  -- | the database type e.g. Postgres, MySQL, SQLite
  Database ->
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
createConnPool db connectString connectFun idle numConns = newPool poolConfig
  where
    freshConnection :: IO Conn
    freshConnection = connect db <$> connectFun connectString
    poolConfig :: PoolConfig Conn
    poolConfig = defaultPoolConfig freshConnection disconnect idle numConns
