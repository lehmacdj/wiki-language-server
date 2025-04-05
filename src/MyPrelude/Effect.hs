module MyPrelude.Effect
  ( module MyPrelude.Effect,
    module X,
  )
where

import Control.Concurrent.Thread.Delay
import Control.Exception.Safe as X
import Data.Text.Encoding (decodeUtf8')
import Data.Time
import Effectful as X
import Effectful.Concurrent as X hiding (throwTo)
import Effectful.Dispatch.Dynamic as X
import Effectful.Dispatch.Static as X
import Effectful.Dispatch.Static.Primitive
import Effectful.Error.Static as X
import Effectful.FileSystem
import Effectful.FileSystem as X (FileSystem)
import Effectful.FileSystem.IO.ByteString as X (readFile, writeFile)
import Effectful.Reader.Dynamic as X
import Effectful.TH as X
import MyPrelude.Effect.Input as X
import MyPrelude.RestrictedClassyPrelude

type EffectfulEnv es = Env es

untry :: (Exception e) => Either e a -> Eff es a
untry = either throwIO pure

readFileUtf8 :: (HasCallStack, FileSystem :> es) => FilePath -> Eff es Text
readFileUtf8 = (either throwIO pure . decodeUtf8') <=< readFile

writeFileUtf8 :: (FileSystem :> es) => FilePath -> Text -> Eff es ()
writeFileUtf8 path = writeFile path . encodeUtf8

sleep :: (Concurrent :> es) => NominalDiffTime -> Eff es ()
sleep =
  unsafeEff_
    . delay
    . floor @_ @Integer
    . (* 1_000_000)
    . nominalDiffTimeToSeconds
