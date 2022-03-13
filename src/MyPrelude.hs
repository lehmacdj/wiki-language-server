module MyPrelude
  ( -- * Based heavily on ClassyPrelude
    module ClassyPrelude,

    -- * Custom exports, in this module
    codiagonal,
    note,
    noteM,
    onNothing,
    onLeft,
    onRight,

    -- * Various other things; re-exported
    module X,
  )
where

import ClassyPrelude
import Control.Arrow as X (left, right, (<<<), (>>>))
import Control.Lens as X
  ( Fold,
    Getter,
    Lens,
    Lens',
    Prism,
    Prism',
    Setter,
    Setter',
    Traversal,
    Traversal',
    has,
    hasn't,
    over,
    preview,
    set,
    to,
    toListOf,
    view,
    (.~),
    (^.),
    (^..),
    (^?),
    _Just,
    _Left,
    _Nothing,
    _Right,
  )
import Control.Monad.Error.Class as X (MonadError (..))
import Control.Monad.Except as X (ExceptT (..), runExceptT)
import Control.Monad.Reader.Class as X
import Control.Monad.State as X
  ( State,
    StateT (..),
    evalState,
    evalStateT,
    execState,
    execStateT,
    runState,
    runStateT,
  )
import Control.Monad.State.Class as X
import Data.Default as X (Default (..))
import Data.Either as X (fromLeft, fromRight)
import Data.Generics.Labels ()
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Void as X (Void, absurd)
import GHC.Stack as X (HasCallStack)
import GHC.TypeLits as X

-- | Throw an error in place of Nothing. So named because generally the
-- exception will describe the error that took place causing a result of
-- Nothing embedding it in a larger class of possible exceptions.
note :: MonadError e m => e -> Maybe a -> m a
note = noteM . pure

-- | Like noteM, but allows running monadic effects while generating the
-- exception to throw. This is useful in cases when, for example, one wants to
-- do some logging as well (or in the case of an LSP implementation report a
-- diagnostic) as well.
--
-- Also consider using @onNothing@ instead which can be more idomatic when the
-- thing that is maybe is short and you want to have an anonymous error
-- handling block.
noteM :: MonadError e m => m e -> Maybe a -> m a
noteM err = \case
  Nothing -> throwError =<< err
  Just x -> pure x

onNothing :: Applicative m => Maybe a -> m a -> m a
onNothing = flip (`maybe` pure)

onLeft :: Applicative m => Either e a -> (e -> m a) -> m a
onLeft = flip (`either` pure)

onRight :: Applicative m => Either a e -> (e -> m a) -> m a
onRight = flip (either pure)

codiagonal :: Either a a -> a
codiagonal = \case
  Left a -> a
  Right a -> a
