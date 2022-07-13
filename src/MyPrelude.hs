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
    onRight,
    foldMapA,
    r,

    -- * EarlyReturnT and pals
    EarlyReturnT,
    returnEarly,
    runEarlyReturnT,

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
import Data.Typeable (typeOf)
import Data.Void as X (Void, absurd)
import GHC.Stack as X (HasCallStack)
import GHC.TypeLits as X
import Language.Haskell.TH.Quote
  ( QuasiQuoter
      ( QuasiQuoter,
        quoteDec,
        quoteExp,
        quotePat,
        quoteType
      ),
  )
import Language.Haskell.TH.Syntax
import Orphans ()
import Prelude as X (showChar, showParen, showString, shows)

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

onNothingM :: Monad m => m (Maybe a) -> m a -> m a
onNothingM action err = action >>= \m -> onNothing m err

onLeft :: Applicative m => Either e a -> (e -> m a) -> m a
onLeft = flip (`either` pure)

onRight :: Applicative m => Either a e -> (e -> m a) -> m a
onRight = flip (either pure)

codiagonal :: Either a a -> a
codiagonal = \case
  Left a -> a
  Right a -> a

-- | Branch over a 'Foldable' collection of values using the supplied
--   action.
--
-- Taked from Agda-2.6.2.2 and modified to support MonoFoldable
foldMapA :: (Alternative f, MonoFoldable t) => (Element t -> f b) -> t -> f b
foldMapA f = getAlt . foldMap (Alt . f)

newtype EarlyReturnT r m a = EarlyReturnT {unEarlyReturnT :: m a}
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
      MonadReader x
    )

instance MonadTrans (EarlyReturnT r) where
  lift = EarlyReturnT

-- There might be a problem here with scoping if one has multiple EarlyReturnT's
-- because each kind of exception isn't unique. It would be possible to fix this
-- with a phantom type parameter but because this is an extreme edge case I'm
-- too lazy to figure it out exactly
newtype EarlyReturn r = EarlyReturn r
  deriving anyclass (Exception)

instance forall r. Typeable r => Show (EarlyReturn r) where
  showsPrec p _ =
    showParen (p > 10) $
      showString "EarlyReturn "
        . shows (typeOf (error "unused arg to typeOf" :: EarlyReturn r))

returnEarly :: (MonadIO m, Typeable r) => r -> EarlyReturnT r m a
returnEarly = throwIO . EarlyReturn

runEarlyReturnT :: (MonadUnliftIO m, Typeable r) => EarlyReturnT r m r -> m r
runEarlyReturnT action =
  unEarlyReturnT $
    action `catch` \(EarlyReturn r) -> pure r

-- |
--
-- A quasiquoter for raw string literals - that is, string literals that don't
-- recognise the standard escape sequences (such as @\'\\n\'@). Basically, they
-- make your code more readable by freeing you from the responsibility to escape
-- backslashes. They are useful when working with regular expressions, DOS/Windows
-- paths and markup languages (such as XML).
--
-- Don't forget the @LANGUAGE QuasiQuotes@ pragma if you're using this
-- module in your code.
--
-- Usage:
--
-- @
--     ghci> :set -XQuasiQuotes
--     ghci> import Text.RawString.QQ
--     ghci> let s = [r|\\w+\@[a-zA-Z_]+?\\.[a-zA-Z]{2,3}|]
--     ghci> s
--     \"\\\\w+\@[a-zA-Z_]+?\\\\.[a-zA-Z]{2,3}\"
--     ghci> [r|C:\\Windows\\SYSTEM|] ++ [r|\\user32.dll|]
--     \"C:\\\\Windows\\\\SYSTEM\\\\user32.dll\"
-- @
--
-- Multiline raw string literals are also supported:
--
-- @
--     multiline :: String
--     multiline = [r|\<HTML\>
--     \<HEAD\>
--     \<TITLE\>Auto-generated html formated source\</TITLE\>
--     \<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=windows-1252\"\>
--     \</HEAD\>
--     \<BODY LINK=\"#0000ff\" VLINK=\"#800080\" BGCOLOR=\"#ffffff\"\>
--     \<P\> \</P\>
--     \<PRE\>|]
-- @
--
-- Caveat: since the @\"|]\"@ character sequence is used to terminate the
-- quasiquotation, you can't use it inside the raw string literal. Use 'rQ' if you
-- want to embed that character sequence inside the raw string.
--
-- For more on raw strings, see e.g.
-- <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2006/n2053.html>
--
-- For more on quasiquotation, see
-- <http://www.haskell.org/haskellwiki/Quasiquotation>
--
-- Taken from raw-strings-qq
r :: QuasiQuoter
r =
  QuasiQuoter
    { -- Extracted from dead-simple-json.
      quoteExp = return . LitE . StringL . normaliseNewlines,
      quotePat = \_ ->
        fail
          "illegal raw string QuasiQuote \
          \(allowed as expression only, used as a pattern)",
      quoteType = \_ ->
        fail
          "illegal raw string QuasiQuote \
          \(allowed as expression only, used as a type)",
      quoteDec = \_ ->
        fail
          "illegal raw string QuasiQuote \
          \(allowed as expression only, used as a declaration)"
    }
  where
    -- See https://github.com/23Skidoo/raw-strings-qq/issues/1 and
    -- https://ghc.haskell.org/trac/ghc/ticket/11215.
    normaliseNewlines :: String -> String
    normaliseNewlines [] = []
    normaliseNewlines ('\r' : '\n' : cs) = '\n' : normaliseNewlines cs
    normaliseNewlines (c : cs) = c : normaliseNewlines cs
