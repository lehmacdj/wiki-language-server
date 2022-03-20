{-# LANGUAGE DeriveLift #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances to make Pandoc an instance of Lift so that it may be used
-- in template haskell code
module Orphans.PandocLift where

import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)
import Text.Pandoc.Definition

deriving instance Lift Alignment

deriving instance Lift Block

deriving instance Lift Caption

deriving instance Lift Cell

deriving instance Lift Citation

deriving instance Lift CitationMode

deriving instance Lift ColSpan

deriving instance Lift ColWidth

deriving instance Lift Format

deriving instance Lift Inline

deriving instance Lift ListNumberDelim

deriving instance Lift ListNumberStyle

deriving instance Lift MathType

deriving instance Lift Meta

deriving instance Lift MetaValue

deriving instance Lift Pandoc

deriving instance Lift QuoteType

deriving instance Lift Row

deriving instance Lift RowHeadColumns

deriving instance Lift RowSpan

deriving instance Lift TableBody

deriving instance Lift TableFoot

deriving instance Lift TableHead
