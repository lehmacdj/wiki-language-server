{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MyPrelude.Orphans where

import ClassyPrelude
import Control.Monad.Trans
import Language.LSP.Server
import MyPrelude.Orphans.LspTypes ()
import MyPrelude.Orphans.PandocLift ()

instance
  {-# OVERLAPPABLE #-}
  (MonadUnliftIO (t m), MonadTrans t, MonadLsp c m) =>
  MonadLsp c (t m)
  where
  getLspEnv = lift getLspEnv
