module MyPrelude
  ( -- * Based heavily on ClassyPrelude
    module ClassyPrelude,

    -- * Custom exports, in this module
    codiagonal,
    note,
    noteM,

    -- * Various other things we want to export
    module X,
  )
where

import ClassyPrelude
import Control.Lens as X
  ( over,
    set,
    to,
    view,
    (.~),
    (^.),
    _Just,
    _Left,
    _Nothing,
    _Right,
  )
import Control.Monad.Error.Class as X (MonadError (..))
import Control.Monad.Except as X (ExceptT (..), runExceptT)
import Control.Monad.Reader.Class as X
import Data.Default as X (Default (..))
import Data.Generics.Labels ()
import Data.List.NonEmpty as X (NonEmpty (..))
import GHC.TypeLits as X

-- | Throw an error in place of Nothing. So named because generally the
-- exception will describe the error that took place causing a result of
-- Nothing embedding it in a larger class of possible exceptions.
note :: MonadError e m => Maybe a -> e -> m a
note v = noteM v . pure

-- | Like noteM, but flipped and allows running monadic effects while
-- generating the exception to throw. This is useful in cases when, for
-- example, one wants to do some logging as well (or in the case of an LSP
-- implementation report a diagnostic) as well.
noteM :: MonadError e m => Maybe a -> m e -> m a
noteM v err = case v of
  Nothing -> throwError =<< err
  Just x -> pure x

codiagonal :: Either a a -> a
codiagonal = \case
  Left a -> a
  Right a -> a
