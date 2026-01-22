module MyPrelude.Testing
  ( -- * Custom assertions
    shouldBeMultiline,

    -- * Golden tests
    goldenTest,
    goldenTestBinary,
    goldenTestShow,

    -- * Running tests interactively
    runTests,
    runSpec,

    -- * Testing parsers
    parseForTest,
    testParserParses,
    testParserFails,

    -- * Re-exports
    module X,
  )
where

import GHC.Stack
import MyPrelude.RestrictedClassyPrelude
import Test.Hspec as X (Spec, describe, it)
import Test.Hspec.Expectations as X
import Test.Tasty as X
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.HUnit as X hiding (assert)
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.QuickCheck as X hiding (Fixed (..), label)
import Text.Megaparsec

parseForTest ::
  (HasCallStack, ShowErrorComponent e) =>
  String -> Parsec e Text a -> Text -> IO a
parseForTest whatToParse parser input =
  case parse (parser <* eof) ("<" <> whatToParse <> ">") input of
    Left err -> do
      expectationFailure $ "Failed to parse: " <> errorBundlePretty err
      error "unreachable"
    Right result -> pure result

testParserParses ::
  (Eq a, Show a, HasCallStack, ShowErrorComponent e) =>
  Parsec e Text a -> Text -> a -> Assertion
testParserParses parser string expected =
  case parse (parser <* eof) "<test>" string of
    Right actual -> expected @=? actual
    Left err ->
      assertFailure
        $ ("expected: " ++ show expected ++ "\n")
        ++ ("but parser failed with:\n" ++ errorBundlePretty err)

testParserFails ::
  (Eq a, Show a, HasCallStack) => Parsec e Text a -> Text -> Assertion
testParserFails parser string =
  case parse parser "<test>" string of
    Right x ->
      assertFailure
        $ "expected parser to fail, but it succeeded producing: "
        ++ show x
    Left _ -> pure ()

-- | Convenience export for running tests in GHCi with a more ergonomic name
runTests :: TestTree -> IO ()
runTests = defaultMain

runSpec :: Spec -> IO ()
runSpec s = defaultMain =<< testSpec "<interactive>" s

newtype NewlineBeforeShow a = NewlineBeforeShow {underlying :: a}
  deriving (Eq)

instance (Show a) => Show (NewlineBeforeShow a) where
  show a = "\n" ++ show a.underlying

-- | The default output of `shouldBe` isn't ideal when the show instance is
-- multiline, e.g.:
-- ```
-- expected: @1{out=[@2 via "a"]}
-- @2{out=[@3 via "b"]}
-- @3{out=[@5 via "c"]}
-- @4{out=[@5 via "d"]}
-- @5{out=[]}
-- but got: @1{out=[]}
-- @2{out=[]}
-- @3{out=[]}
-- @4{out=[@5 via "d"]}
-- @5{out=[]}
-- ```
--
-- This prepends newlines before the show instances in the output
shouldBeMultiline :: (HasCallStack, Eq a, Show a) => a -> a -> Expectation
shouldBeMultiline actual expected = do
  NewlineBeforeShow actual `shouldBe` NewlineBeforeShow expected

-- | Creates a golden test for binary data by comparing the ByteString with a
-- file in a subtree of test/ relative to the module the function is called from.
--
-- For example, a test defined in the module @Utils.Testing@ created like:
-- @goldenTestBinary "asdf" someValue@
-- would have its golden file located at @test/Utils/Testing/asdf@
--
-- The test name will be the basename of the test file (the first argument).
goldenTestBinary :: (HasCallStack) => String -> ByteString -> TestTree
goldenTestBinary testName actualBytes =
  goldenVsStringDiff
    testName
    (\ref new -> ["diff", "-u", ref, new])
    (goldenPathFor callStack testName)
    (pure $ fromStrict actualBytes)

-- | Creates a golden test for Text by comparing the Text with a
-- file in a subtree of test/ relative to the module the function is called from.
--
-- For example, a test defined in the module @Models.Example@ created like:
-- @goldenTest "asdf" someText@
-- would have its golden file located at @test/Models/Example/asdf@
goldenTest :: (HasCallStack) => String -> Text -> TestTree
goldenTest testName actualText =
  goldenTestBinary testName (encodeUtf8 actualText)

-- | Creates a golden test for any showable value by comparing its shown representation
-- with a file in a subtree of test/ relative to the module the function is called from.
--
-- For example, a test defined in the module @Models.Example@ created like:
-- @goldenTestShow "asdf" someValue@
-- would have its golden file located at @test/Models/Example/asdf@
goldenTestShow :: (HasCallStack, Show a) => String -> a -> TestTree
goldenTestShow testName value =
  goldenTest testName (tshow value)

-- | Construct the golden file path for a test, based on the calling module
goldenPathFor :: CallStack -> String -> FilePath
goldenPathFor cs testName =
  let callingModule = getCallingModule cs
      modPath = map (\c -> if c == '.' then '/' else c) callingModule
   in "test" </> modPath </> testName

-- | Extract the module name from the call stack.
-- We skip entries that are from Utils.Testing itself to find the actual calling module.
getCallingModule :: CallStack -> String
getCallingModule cs =
  case filter (not . isUtilsTestingModule . srcLocModule . snd) (getCallStack cs) of
    (_, loc) : _ -> srcLocModule loc
    [] -> error "goldenTest: unable to determine calling module from callstack"
  where
    isUtilsTestingModule modName = modName == "MyPrelude.Testing"
