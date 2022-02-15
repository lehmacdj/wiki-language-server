module MyPrelude
  ( -- * Based heavily on ClassyPrelude
    module ClassyPrelude,

    -- * Custom exports, in this module

    -- * Various other things we want to export
    module X,
  )
where

import ClassyPrelude
import Control.Lens as X (set, view, (.~), (^.))
import Control.Monad.Reader.Class as X
import Data.Default as X (Default (..))
import Data.Generics.Labels ()
