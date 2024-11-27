{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Database.SQLite.Simple as SQLite
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Database.MySQL.Simple as MySQL
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Time (Day, LocalTime)
import Data.Int (Int64)
import Data.Aeson (Value)
import GHC.Generics (Generic)


-- | Convertible typeclass for conversion between Haskell types and SqlValue
class Convertible a where
    toSqlValue :: a -> SqlValue
    fromSqlValue :: SqlValue -> Maybe a

-- | Convertible Instances for common types
instance Convertible Int64 where
    toSqlValue = SqlInt
    fromSqlValue (SqlInt i) = Just i
    fromSqlValue _ = Nothing

instance Convertible Double where
    toSqlValue = SqlDouble
    fromSqlValue (SqlDouble d) = Just d
    fromSqlValue _ = Nothing

instance Convertible Text where
    toSqlValue = SqlText
    fromSqlValue (SqlText t) = Just t
    fromSqlValue _ = Nothing

instance Convertible ByteString where
    toSqlValue = SqlBlob
    fromSqlValue (SqlBlob b) = Just b
    fromSqlValue _ = Nothing

instance Convertible Bool where
    toSqlValue = SqlBool
    fromSqlValue (SqlBool b) = Just b
    fromSqlValue _ = Nothing

instance Convertible Day where
    toSqlValue = SqlDate
    fromSqlValue (SqlDate d) = Just d
    fromSqlValue _ = Nothing

instance Convertible LocalTime where
    toSqlValue = SqlTimestamp
    fromSqlValue (SqlTimestamp ts) = Just ts
    fromSqlValue _ = Nothing

instance Convertible () where
    toSqlValue _ = SqlNull
    fromSqlValue SqlNull = Just ()
    fromSqlValue _ = Nothing

-- | Abstract Row Type
type Row = [(Text, SqlValue)]

-- | Database Typeclass
class Database conn where
    connect :: Text -> IO conn                -- Connect to the database
    disconnect :: conn -> IO ()              -- Disconnect from the database
    execute :: conn -> Text -> [SqlValue] -> IO () -- Execute a statement with parameters
    query :: conn -> Text -> [SqlValue] -> IO [Row] -- Run a query with parameters and get results

-- | SQLite Implementation
instance Database SQLite.Connection where
    connect connStr = SQLite.open (Text.unpack connStr)
    disconnect = SQLite.close
    execute conn stmt params =
        SQLite.execute conn (SQLite.Query stmt) (map toSqlValue params)
    query conn stmt params = do
        rows <- SQLite.query conn (SQLite.Query stmt) (map toSqlValue params) :: IO [[SQLite.Only SqlValue]]
        return $ map (zip ["column1", "column2", ...] . SQLite.fromOnly) rows

-- | PostgreSQL Implementation
instance Database Postgres.Connection where
    connect connStr = Postgres.connectPostgreSQL (encodeUtf8 connStr)
    disconnect = Postgres.close
    execute conn stmt params =
        Postgres.execute conn (Postgres.Query stmt) (map toSqlValue params) >> pure ()
    query conn stmt params = do
        rows <- Postgres.query conn (Postgres.Query stmt) (map toSqlValue params) :: IO [[Postgres.Only SqlValue]]
        return $ map (zip ["column1", "column2", ...] . Postgres.fromOnly) rows

-- | MySQL Implementation
instance Database MySQL.Connection where
    connect connStr = MySQL.connect MySQL.defaultConnectInfo { MySQL.connectDatabase = Text.unpack connStr }
    disconnect = MySQL.close
    execute conn stmt params =
        MySQL.execute conn (MySQL.Query stmt) (map toSqlValue params) >> pure ()
    query conn stmt params = do
        rows <- MySQL.query conn (MySQL.Query stmt) (map toSqlValue params) :: IO [[MySQL.Only SqlValue]]
        return $ map (zip ["column1", "column2", ...] . MySQL.fromOnly) rows

main :: IO ()
main = do
    let connString = "dbname=mydb user=myuser password=mypass"
    conn <- connect connString :: IO Postgres.Connection
    execute conn "INSERT INTO users (name, age) VALUES (?, ?)" [SqlText "Alice", SqlInt 30]
    rows <- query conn "SELECT name, age FROM users WHERE age > ?" [SqlInt 25]
    print rows
    disconnect conn