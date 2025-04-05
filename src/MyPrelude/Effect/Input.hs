module MyPrelude.Effect.Input where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import MyPrelude.RestrictedClassyPrelude

data Input i :: Effect where
  Input :: Input i m i

makeEffect ''Input

inputs :: (Input i :> es) => (i -> a) -> Eff es a
inputs f = f <$> input

runInputViaGetter :: Eff es i -> Eff (Input i : es) a -> Eff es a
runInputViaGetter getter = interpret \_ -> \case
  Input -> getter
