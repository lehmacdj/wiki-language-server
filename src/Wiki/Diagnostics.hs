module Wiki.Diagnostics
  ( DiagnosticKind (..),
    mkDiagnostic,
    onLine,
    atLineCol,

    -- * Re-exports from Language.LSP.Types
    module X,
  )
where

import Language.LSP.Types as X
  ( Diagnostic (..),
    DiagnosticRelatedInformation (..),
    DiagnosticSeverity (..),
    DiagnosticSource,
    DiagnosticTag (..),
    List (..),
    Location (..),
    Position (..),
    Range (..),
    Uri (..),
    type (|?) (..),
  )
import Language.LSP.Types.Lens as X
  ( HasLocation (..),
    HasMessage (..),
    HasRange (..),
    HasRelatedInformation (..),
    HasUri (..),
  )
import MyPrelude
import Prettyprinter
import Prettyprinter as X (pretty)
import Prettyprinter.Render.Text

data DiagnosticKind
  = InternalError
  | ParseError
  | GeneralInfo

kindSourceName :: DiagnosticKind -> DiagnosticSource
kindSourceName = const "Wiki Language Server"

wlsCode :: Int -> Text
wlsCode = ("WLS" <>) . tshow

kindCode :: DiagnosticKind -> Text
kindCode = \case
  InternalError -> wlsCode 1
  ParseError -> wlsCode 2
  GeneralInfo -> wlsCode 3

kindSeverity :: DiagnosticKind -> DiagnosticSeverity
kindSeverity = \case
  InternalError -> DsError
  ParseError -> DsError
  GeneralInfo -> DsInfo

kindTags :: DiagnosticKind -> [DiagnosticTag]
kindTags = \case
  InternalError -> []
  ParseError -> []
  GeneralInfo -> []

onLine :: Int -> Range
onLine line = Range (Position line 0) (Position (line + 1) 0)

-- | For cases where we don't have a start and an end point start the range at
-- the current location, and go until the start of the next line, as this is
-- the best estimate of the precise range of the error that we can give.
atLineCol :: Int -> Int -> Range
atLineCol line col = Range (Position line col) (Position (line + 1) 0)

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
      _source = Just $ kindSourceName k,
      _message = renderStrict . layoutLspDiagnostic $ m,
      _tags = Just . List $ kindTags k,
      _relatedInformation = Nothing
    }
