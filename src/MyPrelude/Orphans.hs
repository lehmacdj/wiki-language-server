{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MyPrelude.Orphans where

import Control.Monad.Trans
import Data.IxSet.Typed
import Language.LSP.Server
import MyPrelude.Orphans.LspTypes ()
import MyPrelude.Orphans.PandocLift ()
import MyPrelude.RestrictedClassyPrelude

instance
  {-# OVERLAPPABLE #-}
  (MonadUnliftIO (t m), MonadTrans t, MonadLsp c m) =>
  MonadLsp c (t m)
  where
  getLspEnv = lift getLspEnv

type instance Element (IxSet ixs a) = a

instance MonoFoldable (IxSet ixs a)

instance (Indexable ixs a) => MonoPointed (IxSet ixs a) where
  opoint = Data.IxSet.Typed.fromList . pure
