module MyPrelude
  ( -- * Based heavily on ClassyPrelude
    module ClassyPrelude,

    -- * Misc custom exports, in this module
    codiagonal,
    note,
    noteM,
    onNothing,
    onNothingM,
    onLeft,
    onLeft_,
    onRight,
    onRight_,
    justFromLeft,
    justFromRight,
    foldMapA,

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
    at,
    filtered,
    filteredBy,
    has,
    hasn't,
    ix,
    only,
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
    _1,
    _2,
    _3,
    _4,
    _5,
    _Just,
    _Left,
    _Nothing,
    _Right,
    _head,
  )
import Control.Monad as X (MonadFail (fail))
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
import Control.Monad.Trans.Class as X (MonadTrans (..))
import Data.Default as X (Default (..))
import Data.Either as X (fromLeft, fromRight)
import Data.Generics.Labels ()
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Monoid (Alt (Alt, getAlt))
import Data.Proxy as X (Proxy (..))
import Data.Void as X (Void, absurd)
import GHC.TypeLits as X
import MyPrelude.EarlyReturn as X
import MyPrelude.Effect as X
import MyPrelude.ExceptionErrorT as X
import MyPrelude.Orphans ()
import MyPrelude.RawStrings as X
import MyPrelude.Testing as X

-- | Throw an error in place of Nothing. So named because generally the
-- exception will describe the error that took place causing a result of
-- Nothing embedding it in a larger class of possible exceptions.
note :: (MonadError e m) => e -> Maybe a -> m a
note = noteM . pure

-- | Like noteM, but allows running monadic effects while generating the
-- exception to throw. This is useful in cases when, for example, one wants to
-- do some logging as well (or in the case of an LSP implementation report a
-- diagnostic) as well.
--
-- Also consider using @onNothing@ instead which can be more idomatic when the
-- thing that is maybe is short and you want to have an anonymous error
-- handling block.
noteM :: (MonadError e m) => m e -> Maybe a -> m a
noteM err = \case
  Nothing -> throwError =<< err
  Just x -> pure x

onNothing :: (Applicative m) => Maybe a -> m a -> m a
onNothing = flip (`maybe` pure)

onNothingM :: (Monad m) => m (Maybe a) -> m a -> m a
onNothingM action err = action >>= \m -> onNothing m err

onLeft :: (Applicative m) => Either e a -> (e -> m a) -> m a
onLeft = flip (`either` pure)

onLeft_ :: (Applicative m) => Either e a -> m a -> m a
onLeft_ e = onLeft e . const

onRight :: (Applicative m) => Either a e -> (e -> m a) -> m a
onRight = flip (either pure)

onRight_ :: (Applicative m) => Either a e -> m a -> m a
onRight_ e = onRight e . const

codiagonal :: Either a a -> a
codiagonal = \case
  Left a -> a
  Right a -> a

justFromRight :: Either a b -> Maybe b
justFromRight = either (const Nothing) Just

justFromLeft :: Either a b -> Maybe a
justFromLeft = either Just (const Nothing)

-- | Branch over a 'Foldable' collection of values using the supplied
--   action.
--
-- Taked from Agda-2.6.2.2 and modified to support MonoFoldable
foldMapA :: (Alternative f, MonoFoldable t) => (Element t -> f b) -> t -> f b
foldMapA f = getAlt . foldMap (Alt . f)
