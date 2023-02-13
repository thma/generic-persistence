{-# Language 
  DefaultSignatures, 
  FlexibleContexts, 
  DeriveAnyClass,
  StandaloneDeriving #-}
module Database.GP.Generics(
  ToRow,
  toRow,
  GToRow,
  gtoRow,
  ToSql,
  toSqlValue,
  main,
) where

import GHC.Generics
import Database.HDBC
--import Database.HDBC.SqlValue
import Data.Int
import Data.Word

class ToRow a where
  toRow :: a -> [SqlValue]

  default toRow :: Generic a => GToRow (Rep a) => a -> [SqlValue]
  toRow a = gtoRow $ from a

class GToRow f where
  gtoRow :: (f a) -> [SqlValue]

instance GToRow U1 where
  gtoRow U1 = mempty

instance ToSql a => GToRow (K1 i a) where
  gtoRow (K1 a) = pure $ toSqlValue a

instance (GToRow a, GToRow b) => GToRow (a :*: b) where
  gtoRow (a :*: b) = gtoRow a `mappend` gtoRow b

instance GToRow a => GToRow (M1 i c a) where
  gtoRow (M1 a) = gtoRow a

-- | A type that may be used as a single parameter to a SQL query.
class ToSql a where
    toSqlValue :: a -> SqlValue

instance ToSql SqlValue where
    toSqlValue a = a
    {-# INLINE toSqlValue #-}

-- instance (ToSql a) => ToSql (Maybe a) where
--     toSqlValue Nothing  = Base.SQLNull
--     toSqlValue (Just a) = toSqlValue a
--     {-# INLINE toSqlValue #-}

-- instance ToSql Null where
--     toSqlValue _ = Base.SQLNull
--     {-# INLINE toSqlValue #-}

instance ToSql Bool where
    toSqlValue False = SqlInteger 0
    toSqlValue True  = SqlInteger 1
    {-# INLINE toSqlValue #-}

instance ToSql Int8 where
    toSqlValue = SqlInteger . fromIntegral
    {-# INLINE toSqlValue #-}

instance ToSql Int16 where
    toSqlValue = SqlInteger . fromIntegral
    {-# INLINE toSqlValue #-}

instance ToSql Int32 where
    toSqlValue = SqlInteger . fromIntegral
    {-# INLINE toSqlValue #-}

instance ToSql Int where
    toSqlValue = SqlInteger . fromIntegral
    {-# INLINE toSqlValue #-}

instance ToSql Int64 where
    toSqlValue = SqlInteger . fromIntegral
    {-# INLINE toSqlValue #-}

instance ToSql Integer where
    toSqlValue = SqlInteger
    {-# INLINE toSqlValue #-}

instance ToSql Word8 where
    toSqlValue = SqlInteger . fromIntegral
    {-# INLINE toSqlValue #-}

instance ToSql Word16 where
    toSqlValue = SqlInteger . fromIntegral
    {-# INLINE toSqlValue #-}

instance ToSql Word32 where
    toSqlValue = SqlInteger . fromIntegral
    {-# INLINE toSqlValue #-}

instance ToSql Word where
    toSqlValue = SqlInteger . fromIntegral
    {-# INLINE toSqlValue #-}

instance ToSql Word64 where
    toSqlValue = SqlInteger . fromIntegral
    {-# INLINE toSqlValue #-}

instance ToSql Float where
    toSqlValue = SqlDouble . realToFrac
    {-# INLINE toSqlValue #-}

instance ToSql Double where
    toSqlValue = SqlDouble
    {-# INLINE toSqlValue #-}

-- instance ToSql SB.ByteString where
--     toSqlValue = SQLBlob
--     {-# INLINE toSqlValue #-}

-- instance ToSql LB.ByteString where
--     toSqlValue = toSqlValue . SB.concat . LB.toChunks
--     {-# INLINE toSqlValue #-}

-- instance ToSql T.Text where
--     toSqlValue = SQLText
--     {-# INLINE toSqlValue #-}

instance ToSql String where
    toSqlValue = SqlString
    {-# INLINE toSqlValue #-}

-- instance ToSql LT.Text where
--     toSqlValue = toSqlValue . LT.toStrict
--     {-# INLINE toSqlValue #-}

-- instance ToSql UTCTime where
--     toSqlValue = SQLText . T.decodeUtf8 . toByteString . utcTimeToBuilder
--     {-# INLINE toSqlValue #-}

-- instance ToSql Day where
--     toSqlValue = SQLText . T.decodeUtf8 . toByteString . dayToBuilder
--     {-# INLINE toSqlValue #-}

data Person = Person 
  { name :: String, 
    age :: Int 
  }
    deriving (Generic, Show, ToRow)

main :: IO ()
main = do
    let sqlValueList = toRow $ Person "John Doe" 30
    putStrLn $ "SQL values: " ++ show sqlValueList