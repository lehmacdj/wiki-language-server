{-# OPTIONS_GHC -Wno-orphans #-}

module LSP.Raw where

import Language.LSP.Server
import Models.WikiLanguageServerConfig
import MyPrelude

data LSP :: Effect

type instance DispatchOf LSP = Static WithSideEffects

newtype instance StaticRep LSP
  = LSP {unwrapped :: LanguageContextEnv Config}

runLSP ::
  (IOE :> es) =>
  LanguageContextEnv Config -> Eff (LSP : es) a -> Eff es a
runLSP context = evalStaticRep (LSP context)

instance (IOE :> es, LSP :> es) => MonadLsp Config (Eff es) where
  getLspEnv = do
    LSP env <- getStaticRep
    pure env
