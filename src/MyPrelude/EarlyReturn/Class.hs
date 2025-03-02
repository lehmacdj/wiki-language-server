-- | This needs to be broken out to prevent cycles in the module graph.
module MyPrelude.EarlyReturn.Class where

class MonadEarlyReturn r m where
  returnEarly :: r -> m a
