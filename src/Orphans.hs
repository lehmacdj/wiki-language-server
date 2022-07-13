{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

import ClassyPrelude
import Control.Monad.Trans
import Language.LSP.Server
import Orphans.LspTypes ()
import Orphans.PandocLift ()

instance
  {-# OVERLAPPABLE #-}
  (MonadUnliftIO (t m), MonadTrans t, MonadLsp c m) =>
  MonadLsp c (t m)
  where
  getLspEnv = lift getLspEnv
