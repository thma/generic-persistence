module RIO (
  RIO,
  unRIO,
  runRIO,
  liftRIO,
  MonadIO,
  ask,
  local,
  liftIO,
)

where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader

newtype RIO env a = RIO { unRIO :: ReaderT env IO a }
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader env,MonadThrow)

-- | Using the environment run in IO the action that requires that environment.
--
-- @since 0.0.1.0
runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO env (RIO (ReaderT f)) = liftIO (f env)

-- | Abstract `RIO` to an arbitrary `MonadReader` instance, which can handle IO.
--
-- @since 0.0.1.0
liftRIO :: (MonadIO m, MonadReader env m) => RIO env a -> m a
liftRIO rio = do
  env <- ask
  runRIO env rio
