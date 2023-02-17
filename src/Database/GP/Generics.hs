{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances, ScopedTypeVariables, DefaultSignatures, DeriveAnyClass #-}

module Database.GP.Generics (main) where

import GHC.Generics
import Database.HDBC ( fromSql, toSql, SqlValue )
import Data.Convertible ( convert, ConvertResult, Convertible(..) )
import Data.Kind
import GHC.TypeNats
import Data.Proxy


class ToRow a where
  toRow :: a -> [SqlValue]

  default toRow :: Generic a => GToRow (Rep a) => a -> [SqlValue]
  toRow a = gtoRow $ from a

class GToRow f where
  gtoRow :: f a -> [SqlValue]

instance GToRow U1 where
  gtoRow U1 = mempty

instance (Convertible a SqlValue) => GToRow (K1 i a) where
  gtoRow (K1 a) = pure $ convert a

instance (GToRow a, GToRow b) => GToRow (a :*: b) where
  gtoRow (a :*: b) = gtoRow a `mappend` gtoRow b

instance GToRow a => GToRow (M1 i c a) where
  gtoRow (M1 a) = gtoRow a

----

class FromRow a where
  fromRow :: [SqlValue] -> a

  default fromRow :: Generic a => GFromRow (Rep a) => [SqlValue] -> a
  fromRow = to <$> gfromRow


class GFromRow f where
  gfromRow :: [SqlValue] -> f a

instance GFromRow U1 where
  --gfromRow :: forall k (a :: k). [SqlValue] -> U1 a
  gfromRow = pure U1

instance (Convertible SqlValue a) => GFromRow (K1 i a) where
  --gfromRow :: forall k (a1 :: k). [SqlValue] -> K1 i a a1
  gfromRow = K1 <$> convert . head

instance GFromRow a => GFromRow (M1 i c a) where
  --gfromRow :: forall k (a :: k -> Type) i (c :: Meta) (a1 :: k). GFromRow a => [SqlValue] -> M1 i c a a1
  gfromRow = M1 <$> gfromRow

-- | This instance is the most interesting one. It splits the list of
-- 'SqlValue's into two parts, one for the first field and one for the
-- rest. Then it uses the 'GFromRow' instance for the first field to
-- convert the first part of the list and the 'GFromRow' instance for
-- the rest of the fields to convert the second part of the list.
-- Finally, it combines the two results using the ':*:' constructor.
-- https://stackoverflow.com/questions/75485429/how-to-use-ghc-generics-to-convert-from-product-data-types-to-a-list-of-sqlvalue/75485650#75485650
instance (KnownNat (NumFields f), GFromRow f, GFromRow g) => GFromRow (f :*: g) where
  gfromRow row = gfromRow rowf :*: gfromRow rowg
    where
      (rowf, rowg) = splitAt fNumFields row
      fNumFields = fromIntegral (natVal (Proxy :: Proxy (NumFields f)))

type family NumFields (f :: Type -> Type) :: Nat where
  NumFields (M1 i c f) = 1
  NumFields (f :*: g) = NumFields f + NumFields g



data Person = Person
  { name :: String,
    age :: Int,
    role :: Role
  }
    deriving (Generic, Show, ToRow, FromRow)


data Role = Student | Teacher
  deriving (Generic, Show, Read, Enum)

instance Convertible Role SqlValue where
  safeConvert :: Role -> ConvertResult SqlValue
  safeConvert = Right . toSql . fromEnum

instance Convertible SqlValue Role where
  safeConvert :: SqlValue -> ConvertResult Role
  safeConvert = Right . toEnum . fromSql


main :: IO ()
main = do
    let sqlValueList = toRow $ Person "John Doe" 30 Teacher
    putStrLn $ "SQL values: " ++ show sqlValueList

    let person = fromRow sqlValueList :: Person
    putStrLn $ "Person: " ++ show person