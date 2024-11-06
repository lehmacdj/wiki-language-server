-- | A bunch of utilities to be imported in modules including tests. This
-- doesn't include
module TestPrelude
  ( -- * Custom exports, in this module

    -- * Various other things; re-exported
    module X,
  )
where

import Test.Hspec as X (Spec, describe, it)
import Test.Hspec.Expectations as X
import Test.Tasty as X
import Test.Tasty.HUnit as X
