module MyPrelude.ExceptionErrorT where

import ClassyPrelude
import Control.Monad.Except
import Control.Monad.State
import MyPrelude.EarlyReturn.Class

-- | Like ExceptT but the underlying exception throws and catches exceptions in
-- the IO monad. This allows giving this type an MonadUnliftIO instance.
newtype ExceptionErrorT e m a = ExceptionErrorT {underlying :: m a}
  -- TODO: theoretically it would be nice to add all of the things here,
  -- but there are so many, and in 99.9% of cases this is probably good enough
  -- and we can add more incrementally as needed
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadState x,
      MonadReader x,
      MonadEarlyReturn r
    )

instance MonadTrans (ExceptionErrorT e) where
  lift = ExceptionErrorT

instance (MonadUnliftIO m, Exception e) => MonadError e (ExceptionErrorT e m) where
  throwError e = liftIO $ throwIO e
  catchError (ExceptionErrorT action) continuation =
    ExceptionErrorT $
      action `catch` ((.underlying) . continuation)

runExceptionErrorT ::
  (MonadUnliftIO m, Exception e) => ExceptionErrorT e m a -> m (Either e a)
runExceptionErrorT = try . (.underlying)
