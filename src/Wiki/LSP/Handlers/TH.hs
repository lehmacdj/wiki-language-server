-- | This was an attempt to implement a function that automatically synthesizes
-- a mega handler from a bunch of little handlers. The idea is sound, but
-- implementing this would end up being a lot more trouble than it's worth at
-- the moment.
module Wiki.LSP.Handlers.TH where

-- import Language.Haskell.TH
-- import MyPrelude

-- makeLspHandlers :: Q [Dec]
-- makeLspHandlers = do
--   Loc {loc_filename = filename} <- location
--   when (filename == "<interactive>") $ error "don't run this interactively!"
--   contents <- runIO $ readFile filename
--   -- takeWhile (\x -> isAlphaNum x || x `member` "_'")
--   pure []
