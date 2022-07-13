{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.LspTypes where

import ClassyPrelude
import Language.LSP.Types (ResponseError)

instance Exception ResponseError
