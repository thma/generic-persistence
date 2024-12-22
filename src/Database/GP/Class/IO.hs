{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeFamilies #-}

module Database.GP.Class.IO () where

import           Database.GP.Class                  (MonadGP(..))
import           Database.GP.Conn                   (Conn)
import qualified Database.GP.GenericPersistence  as GP
import           Database.HDBC                      (SqlValue)

-- | Default instance provided for 'IO' using HDBC.
instance MonadGP IO Conn where
  type Id IO Conn = SqlValue
  selectById = GP.selectById
  upsert     = GP.upsert
