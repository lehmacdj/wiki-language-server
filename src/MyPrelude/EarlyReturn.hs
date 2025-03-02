module MyPrelude.EarlyReturn
  ( EarlyReturnT (..),
    withEarlyReturn,
    module X,
  )
where

import ClassyPrelude
import Control.Monad.Except
import Control.Monad.State
import Data.Proxy (Proxy (..))
import Data.Typeable (typeRep)
import MyPrelude.EarlyReturn.Class as X
import Prelude as X (showChar, showParen, showString, shows)

newtype EarlyReturnT r m a = EarlyReturnT {underlying :: m a}
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
      MonadError e
    )

instance MonadTrans (EarlyReturnT r) where
  lift = EarlyReturnT

-- There might be a problem here with scoping if one has multiple EarlyReturnT's
-- because each kind of exception isn't unique. It would be possible to fix this
-- with a phantom type parameter but because this is an extreme edge case I'm
-- too lazy to figure it out exactly
newtype EarlyReturn r = EarlyReturn r
  deriving anyclass (Exception)

instance forall r. (Typeable r) => Show (EarlyReturn r) where
  showsPrec p _ =
    showParen (p > 10) $
      showString "EarlyReturn "
        . shows (typeRep (Proxy @(EarlyReturn r)))

instance (Typeable r, MonadIO m) => MonadEarlyReturn r (EarlyReturnT r m) where
  returnEarly = throwIO . EarlyReturn

withEarlyReturn ::
  (MonadUnliftIO m, Typeable r) => EarlyReturnT r m r -> m r
withEarlyReturn action =
  (.underlying) $
    action `catch` \(EarlyReturn r) -> pure r
