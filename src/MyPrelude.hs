module MyPrelude
  ( -- * Misc custom exports, in this module
    foldMapA,

    -- * Various other things; re-exported
    module X,
  )
where

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
import Data.Functor.Contravariant as X (Contravariant (contramap))
import Data.Generics.Labels ()
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Monoid (Alt (Alt, getAlt))
import Data.Proxy as X (Proxy (..))
import Data.Void as X (Void, absurd)
import GHC.TypeLits as X
import MyPrelude.Effect as X
import MyPrelude.Effect.EarlyReturn as X
import MyPrelude.ExceptionErrorT as X
import MyPrelude.MaybeEither as X
import MyPrelude.Orphans ()
import MyPrelude.RawStrings as X
import MyPrelude.RestrictedClassyPrelude as X
import MyPrelude.Testing as X

-- | Branch over a 'Foldable' collection of values using the supplied
--   action.
--
-- Taked from Agda-2.6.2.2 and modified to support MonoFoldable
foldMapA :: (Alternative f, MonoFoldable t) => (Element t -> f b) -> t -> f b
foldMapA f = getAlt . foldMap (Alt . f)
