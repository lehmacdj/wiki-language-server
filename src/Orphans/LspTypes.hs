{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.LspTypes where

import ClassyPrelude
import Language.LSP.Protocol.Message (ErrorData, MessageKind (..), Method, ResponseError, TResponseError)

instance Exception ResponseError

instance
  forall f (m :: Method f Request).
  (Show (ErrorData m), Typeable f, Typeable m) =>
  Exception (TResponseError m)
