module MyPrelude.Effect
  ( module MyPrelude.Effect,
    module X,
  )
where

import Data.Text.Encoding (decodeUtf8')
import Effectful as X hiding (MonadCatch (..))
import Effectful.Dispatch.Dynamic as X
import Effectful.Dispatch.Static as X
import Effectful.Exception as X hiding (TypeError)
import Effectful.Exception qualified as E
import Effectful.FileSystem
import Effectful.FileSystem.IO.ByteString as X (readFile, writeFile)
import Effectful.TH as X
import MyPrelude.RestrictedClassyPrelude

untry :: (Exception e) => Either e a -> Eff es a
untry = either throwIO pure

readFileUtf8 :: (HasCallStack, FileSystem :> es) => FilePath -> Eff es Text
readFileUtf8 = (>>= either throwIO pure . decodeUtf8') . readFile

writeFileUtf8 :: (FileSystem :> es) => FilePath -> Text -> Eff es ()
writeFileUtf8 path = writeFile path . encodeUtf8
