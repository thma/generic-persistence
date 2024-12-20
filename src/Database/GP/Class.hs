{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

module Database.GP.Class (MonadGP(..), MonadGPConn(..)) where

import           Data.Convertible                   (Convertible)
import           Database.GP.Entity                 (Entity)

-- | Class of monads that support database operations over a given connection.
class Monad m => MonadGP m c where
  type Id m c
  selectById :: (Convertible id (Id m c), Entity a) => c -> id -> m (Maybe a)
  upsert :: Entity a => c -> a -> m ()

-- | Class of monads that can provide a database connection.
class Monad m => MonadGPConn m c where
  askConn :: m c
