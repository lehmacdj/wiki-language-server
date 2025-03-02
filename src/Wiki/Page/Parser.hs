module Wiki.Page.Parser where

import Commonmark
import Commonmark.Extensions
import Commonmark.Pandoc
import MyPrelude
import Text.Pandoc.Builder qualified as Pandoc
import Text.Pandoc.Definition
import Text.Parsec.Error qualified as Parsec
import Text.Parsec.Pos qualified as Parsec
import Wiki.Diagnostics
import Wiki.LSP.Util

parseErrorFromParsec :: Parsec.ParseError -> Diagnostic
parseErrorFromParsec err =
  mkDiagnostic ParseError (atLineCol line col) (pretty (tshow err))
  where
    line = Parsec.sourceLine $ Parsec.errorPos err
    col = Parsec.sourceColumn $ Parsec.errorPos err

toPandoc :: Cm SourceRange Pandoc.Blocks -> Pandoc
toPandoc = Pandoc.doc . unCm

syntaxSpec ::
  (Monad m, Typeable m) =>
  SyntaxSpec m (Cm SourceRange Pandoc.Inlines) (Cm SourceRange Pandoc.Blocks)
syntaxSpec =
  defaultSyntaxSpec
    <> gfmExtensions
    <> wikilinksSpec TitleAfterPipe

-- TODO: parse YAML front matter
parse :: FilePath -> Text -> Either Diagnostic Pandoc
parse filepath t = do
  r <- commonmarkWith syntaxSpec filepath t
  case r of
    Left err -> Left $ parseErrorFromParsec err
    Right v -> Right $ toPandoc v
