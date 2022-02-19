module Wiki.Page.Parser where

import Commonmark qualified
import Commonmark.Extensions qualified as Commonmark
import Commonmark.Pandoc qualified as Commonmark
import MyPrelude
import Text.Pandoc.Definition
import Text.Pandoc.Builder qualified as Pandoc
import Text.Parsec.Error qualified as Parsec
import Text.Parsec.Pos qualified as Parsec
import Wiki.Diagnostics

parseErrorFromParsec :: Parsec.ParseError -> Diagnostic
parseErrorFromParsec err =
  mkDiagnostic ParseError (atLineCol line col) (pretty (tshow err)) where
    line = Parsec.sourceLine $ Parsec.errorPos err
    col = Parsec.sourceColumn $ Parsec.errorPos err

toPandoc :: Commonmark.Cm Commonmark.SourceRange Pandoc.Blocks -> Pandoc
toPandoc = Pandoc.doc . Commonmark.unCm

parse :: FilePath -> Text -> Either Diagnostic Pandoc
parse filepath = bimap parseErrorFromParsec toPandoc
  . Commonmark.commonmark filepath
