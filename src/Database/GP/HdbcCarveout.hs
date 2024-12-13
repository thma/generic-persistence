
module Database.GP.HdbcCarveout where
import Data.Convertible
import Data.Data (Typeable)
import qualified Database.SQLite.Simple as SQLite



class IConnection conn where
  disconnect :: conn -> IO ()
  commit :: conn -> IO ()
  rollback :: conn -> IO ()
  runRaw :: conn -> String -> IO ()
  run :: conn -> String -> [SqlValue] -> IO Integer
  prepare :: conn -> String -> IO Statement

data Statement = Statement
    {
     execute :: [SqlValue] -> IO Integer,
     executeRaw :: IO (),
     executeMany :: [[SqlValue]] -> IO (),
     fetchRow :: IO (Maybe [SqlValue])
    }

data SqlValue = SqlInt Int
              | SqlInteger Integer
              | SqlDouble Double
              | SqlText String
              | SqlBlob String
              | SqlBool Bool
              | SqlDate String
              | SqlTimestamp String
              | SqlNull
              deriving (Show, Eq, Typeable)

instance Convertible SqlValue SqlValue where
    safeConvert = return

{- | Convert a value to an 'SqlValue'.  This function is simply
a restricted-type wrapper around 'convert'.  See extended notes on 'SqlValue'. -}
toSql :: Convertible a SqlValue => a -> SqlValue
toSql = convert


{- | Convert from an 'SqlValue' to a Haskell value.  Any problem is indicated by
   calling 'error'.  This function is simply a restricted-type wrapper around
   'convert'.  See extended notes on 'SqlValue'. -}
fromSql :: Convertible SqlValue a => SqlValue -> a
fromSql = convert

instance Convertible Int SqlValue where
  safeConvert = Right . SqlInt

instance Convertible SqlValue Int where
  safeConvert (SqlInt i) = Right i
  safeConvert x = quickError x

instance Convertible String SqlValue where
  safeConvert = Right . SqlText

instance Convertible SqlValue String where
  safeConvert (SqlText s) = Right s
  safeConvert x = quickError x

quickError :: (Typeable a, Convertible SqlValue a) => SqlValue -> ConvertResult a
quickError = convError "incompatible types"



