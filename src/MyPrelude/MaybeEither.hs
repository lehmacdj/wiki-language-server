module MyPrelude.MaybeEither
  ( module MyPrelude.MaybeEither,
    module X,
  )
where

import Data.Either as X (fromLeft, fromRight)
import MyPrelude.RestrictedClassyPrelude

onNothing :: (Applicative m) => Maybe a -> m a -> m a
onNothing = flip (`maybe` pure)

onNothingM :: (Monad m) => m (Maybe a) -> m a -> m a
onNothingM action err = action >>= \m -> onNothing m err

-- | execute a computation only if it is Just
withJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
withJust (Just x) f = f x
withJust Nothing _ = pure ()

-- | execute a computation only if it is Just
whenJust :: (Applicative m) => Maybe a -> m () -> m ()
whenJust x = withJust x . const

-- | execute a computation only if it is Just
withJustM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
withJustM v action = v >>= (`withJust` action)

-- | execute a computation only if it is Just
whenJustM :: (Monad m) => m (Maybe a) -> m () -> m ()
whenJustM v = withJustM v . const

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
