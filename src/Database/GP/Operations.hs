{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Operations as in 'MonadGP' but without explicit connection passing.
module Database.GP.Operations (selectById, upsert) where

import           Data.Convertible                   (Convertible)
import           Database.GP.Entity                 (Entity)
import           Database.GP.Class                  (Id, MonadGP, MonadGPConn)
import qualified Database.GP.Class               as GP

selectById :: forall m c id a.
  (Convertible id (Id m c), Entity a, MonadGP m c, MonadGPConn m c) =>
  id -> m (Maybe a)
selectById i = GP.askConn @_ @c >>= flip GP.selectById i

upsert :: forall m c a. (Entity a, MonadGP m c, MonadGPConn m c) => a -> m ()
upsert a = GP.askConn @_ @c >>= flip GP.upsert a
