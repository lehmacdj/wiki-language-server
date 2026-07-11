-- | Template Haskell for creating pages (Pandoc data structure) from in a
-- QuasiQuotation
module Models.Page.TH where

import Language.Haskell.Meta.Parse (parseExp)
import Language.Haskell.TH (Exp, Q, listE)
import Language.Haskell.TH.Quote
  ( QuasiQuoter
      ( QuasiQuoter,
        quoteDec,
        quoteExp,
        quotePat,
        quoteType
      ),
  )
import Language.Haskell.TH.Syntax qualified as TH
import Models.Page.Parser qualified as Page
import MyPrelude
import Utils.TH

md :: QuasiQuoter
md =
  QuasiQuoter
    { quoteExp = parseAsExp,
      quotePat = unsupported,
      quoteType = unsupported,
      quoteDec = unsupported
    }
  where
    unsupported _ = fail "unsupported use of QuasiQuoter; only supports Exp context"
    parseAsExp s = do
      t <- trimQuasiQuotation s
      chunks <- parseTemplate t
      if all isLiteral chunks
        then case Page.parse "<QuasiQuoter>" (renderLiteral chunks) of
          Left d -> fail $ d ^. #_message . to unpack
          Right v -> TH.lift v
        else do
          rendered <- renderTemplateExp chunks
          [|
            case Page.parse "<QuasiQuoter>" $(pure rendered) of
              Left d ->
                error $
                  "Invalid interpolated markdown in [md|...|]: "
                    <> unpack (d ^. #_message)
              Right v -> v
            |]

-- | Render a markdown template as text without parsing it. This shares the
-- interpolation and indentation behavior of 'md' and is useful when the
-- markdown is going to be written to a file.
mdText :: QuasiQuoter
mdText =
  QuasiQuoter
    { quoteExp = \s -> trimQuasiQuotation s >>= parseTemplate >>= renderTemplateExp,
      quotePat = unsupported,
      quoteType = unsupported,
      quoteDec = unsupported
    }
  where
    unsupported _ = fail "mdText quasi-quoter only supports expressions"

-- | A parsed piece of a markdown template.
data TemplateChunk = Literal Text | Splice String

isLiteral :: TemplateChunk -> Bool
isLiteral Literal {} = True
isLiteral Splice {} = False

renderLiteral :: [TemplateChunk] -> Text
renderLiteral = foldMap \case
  Literal t -> t
  Splice _ -> error "renderLiteral called with a splice"

renderTemplateExp :: [TemplateChunk] -> Q Exp
renderTemplateExp chunks = do
  pieces <- for chunks \case
    Literal t -> TH.lift t
    Splice expression ->
      case parseExp expression of
        Left err ->
          fail $
            "Parse error in markdown splice: " <> err
        Right expressionExp -> pure expressionExp
  [|mconcat $(listE $ map pure pieces)|]

-- | Parse @$()@ splices. @$$(@ represents a literal @$(@.
parseTemplate :: Text -> Q [TemplateChunk]
parseTemplate = go [] . unpack
  where
    go literal [] = pure $ emitLiteral literal
    go literal ('$' : '$' : '(' : rest) =
      go ('(' : '$' : literal) rest
    go literal ('$' : '(' : rest) = do
      let (expression, remaining) = takeSplice 1 [] rest
      case remaining of
        Nothing -> fail "Unterminated $() splice in markdown quasi-quote"
        Just rest' ->
          (emitLiteral literal <>) . (Splice expression :) <$> go [] rest'
    go literal (c : rest) = go (c : literal) rest

    emitLiteral [] = []
    emitLiteral reversed = [Literal . pack $ reverse reversed]

-- This deliberately understands strings and character literals so parentheses
-- inside them do not terminate a splice. Haskell-src-meta handles the complete
-- expression grammar after the boundary has been found.
takeSplice :: Int -> String -> String -> (String, Maybe String)
takeSplice _ acc [] = (acc, Nothing)
takeSplice depth acc ('"' : rest) =
  let (quoted, remaining) = takeQuoted '"' rest
   in takeSplice depth (acc <> ('"' : quoted)) remaining
takeSplice depth acc ('\'' : rest) =
  let (quoted, remaining) = takeQuoted '\'' rest
   in takeSplice depth (acc <> ('\'' : quoted)) remaining
takeSplice depth acc ('(' : rest) = takeSplice (depth + 1) (acc <> "(") rest
takeSplice 1 acc (')' : rest) = (acc, Just rest)
takeSplice depth acc (')' : rest) = takeSplice (depth - 1) (acc <> ")") rest
takeSplice depth acc (c : rest) = takeSplice depth (acc <> [c]) rest

takeQuoted :: Char -> String -> (String, String)
takeQuoted _ [] = ([], [])
takeQuoted quote ('\\' : c : rest) =
  first (['\\', c] <>) $ takeQuoted quote rest
takeQuoted quote (c : rest)
  | c == quote = ([c], rest)
  | otherwise = first (c :) $ takeQuoted quote rest
