{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell for creating pages (Pandoc data structure) from in a
-- QuasiQuotation
module Wiki.Page.TH where

import Language.Haskell.TH.Quote
  ( QuasiQuoter
      ( QuasiQuoter,
        quoteDec,
        quoteExp,
        quotePat,
        quoteType
      ),
  )
import MyPrelude
import TH.Utils
import Wiki.Page.Parser qualified as Page

md :: QuasiQuoter
md =
  QuasiQuoter
    { quoteExp = parseAsExp,
      quotePat = unsupported,
      quoteType = unsupported,
      quoteDec = unsupported
    }
  where
    unsupported = error "unsupported use of QuasiQuoter; only supports Exp context"
    parseAsExp s = do
      t <- trimQuasiQuotation s
      case Page.parse "<QuasiQuoter>" t of
        Left d -> fail $ d ^. #_message . to unpack
        Right v -> [|v|]
