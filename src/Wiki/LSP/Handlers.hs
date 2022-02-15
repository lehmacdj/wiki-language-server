-- 99.9% of functions in this module have type Handlers m, and it is redundant
-- to specify that every time.
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Handlers for the LSP client methods.
module Wiki.LSP.Handlers (handlers) where

import Language.LSP.Server (Handlers (..))
import MyPrelude

handlers :: Monad m => Handlers m
handlers =
  mconcat
    []
