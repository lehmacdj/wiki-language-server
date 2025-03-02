module Wiki.Diagnostics
  ( DiagnosticKind (..),
    mkDiagnostic,

    -- * Re-exports from Language.LSP.Protocol.Types
    module X,
  )
where

import Language.LSP.Protocol.Lens as X
  ( HasLocation (..),
    HasMessage (..),
    HasRange (..),
    HasRelatedInformation (..),
    HasUri (..),
  )
import Language.LSP.Protocol.Types as X
  ( Diagnostic (..),
    DiagnosticRelatedInformation (..),
    DiagnosticSeverity (..),
    DiagnosticTag (..),
    Location (..),
    Position (..),
    Range (..),
    UInt,
    Uri (..),
    type (|?) (..),
  )
import MyPrelude
import Prettyprinter
import Prettyprinter as X (pretty)
import Prettyprinter.Render.Text

data DiagnosticKind
  = Bug
  | ParseError
  | GeneralInfo

kindSourceName :: DiagnosticKind -> Text
kindSourceName = const "wiki-language-server"

wlsCode :: Int -> Text
wlsCode = ("WLS" <>) . tshow

kindCode :: DiagnosticKind -> Text
kindCode = \case
  Bug -> wlsCode 1
  ParseError -> wlsCode 2
  GeneralInfo -> wlsCode 3

kindSeverity :: DiagnosticKind -> DiagnosticSeverity
kindSeverity = \case
  Bug -> DiagnosticSeverity_Error
  ParseError -> DiagnosticSeverity_Error
  GeneralInfo -> DiagnosticSeverity_Information

kindTags :: DiagnosticKind -> Maybe [DiagnosticTag]
kindTags = \case
  Bug -> Nothing
  ParseError -> Nothing
  GeneralInfo -> Nothing

layoutLspDiagnostic :: Doc ann -> SimpleDocStream ann
layoutLspDiagnostic =
  layoutPretty LayoutOptions {layoutPageWidth = AvailablePerLine 60 1.0}

-- | Create a diagnostic with all relevant fields set. This doesn't populate
-- relatedInformation, if you want to specify relatedInformation modify it
-- manually, after creating the diagnostic with this function.
mkDiagnostic :: DiagnosticKind -> Range -> Doc ann -> Diagnostic
mkDiagnostic k r m =
  Diagnostic
    { _range = r,
      _severity = Just $ kindSeverity k,
      _code = Just . InR $ kindCode k,
      _codeDescription = Nothing, -- add a URI to a wiki describing errors
      _source = Just $ kindSourceName k,
      _message = renderStrict . layoutLspDiagnostic $ m,
      _tags = kindTags k,
      _relatedInformation = Nothing,
      _data_ = Nothing
    }
