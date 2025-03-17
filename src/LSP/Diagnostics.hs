module LSP.Diagnostics where

import LSP.Raw
import Language.LSP.Diagnostics
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Models.WikiLanguageServerConfig
import MyPrelude

data Diagnostics :: Effect where
  SendDiagnostics :: NormalizedUri -> Maybe Int32 -> [Diagnostic] -> Diagnostics m ()

makeEffect ''Diagnostics

sendDiagnostics_ ::
  (LSP :> es, IOE :> es) => NormalizedUri -> Maybe Int32 -> [Diagnostic] -> Eff es ()
sendDiagnostics_ uri version diagnostics = do
  Config {maxDiagnostics} <- getConfig
  publishDiagnostics maxDiagnostics uri version (partitionBySource diagnostics)

runDiagnostics :: (LSP :> es, IOE :> es) => Eff (Diagnostics : es) a -> Eff es a
runDiagnostics = interpret_ \case
  SendDiagnostics uri version diagnostics -> sendDiagnostics_ uri version diagnostics
