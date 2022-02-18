module Wiki.Page.Parser where

import Commonmark
import Commonmark.Extensions
import MyPrelude
import Text.Pandoc.Definition

parse :: ByteString -> Either ParseError Pandoc
parse = error "unimplemented"
