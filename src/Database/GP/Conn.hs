module Database.GP.Conn (
  Conn (..),
  connect, 
  Database (..),
) 
where

import          Database.HDBC hiding (withWConn)
import Control.Monad ((>=>))

data Database = Postgres | MySQL | SQLite | Oracle | MSSQL | Other String
  deriving (Show, Eq)

data Conn = forall conn. IConnection conn => 
  Conn 
    { db ::Database, 
      implicitCommit :: Bool,
      connection :: conn
    }

connect :: forall conn. IConnection conn => Database -> conn -> Conn
connect db = Conn db True 

withWConn :: forall b. Conn -> (forall conn. IConnection conn => conn -> b) -> b
withWConn conn f =
    case conn of
         Conn _db _ec x -> f x

instance IConnection Conn where
  disconnect w = withWConn w disconnect
  commit w = withWConn w commit
  rollback w = withWConn w rollback
  runRaw w = withWConn w runRaw
  run w = withWConn w run
  prepare w = withWConn w prepare
  clone w@(Conn db ec _) = withWConn w (clone >=> return . Conn db ec)
  hdbcDriverName w = withWConn w hdbcDriverName
  hdbcClientVer w = withWConn w hdbcClientVer
  proxiedClientName w = withWConn w proxiedClientName
  proxiedClientVer w = withWConn w proxiedClientVer
  dbServerVer w = withWConn w dbServerVer
  dbTransactionSupport w = withWConn w dbTransactionSupport
  getTables w = withWConn w getTables
  describeTable w = withWConn w describeTable