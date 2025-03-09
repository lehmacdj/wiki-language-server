module MyPrelude.Effect.EarlyReturn where

import Effectful.Error.Static
import MyPrelude.Effect
import MyPrelude.MaybeEither
import MyPrelude.RestrictedClassyPrelude

data EarlyReturn r :: Effect where
  ReturnEarly :: r -> EarlyReturn r m a

makeEffect ''EarlyReturn

withEarlyReturn_ :: Eff '[EarlyReturn r] r -> r
withEarlyReturn_ = runPureEff . withEarlyReturn

withEarlyReturn :: Eff (EarlyReturn r : es) r -> Eff es r
withEarlyReturn = reinterpret_ (fmap codiagonal . runErrorNoCallStack) \case
  ReturnEarly r -> throwError_ r
