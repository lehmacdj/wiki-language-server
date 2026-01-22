module MyPrelude.Testing.GoldenSpec where

import MyPrelude.RestrictedClassyPrelude
import MyPrelude.Testing

-- Test that the module name is correctly detected
-- (needs to be in a separate module to actually test this properly)
test_moduleDetection :: TestTree
test_moduleDetection =
  testGroup
    "Golden test module detection"
    [ goldenTest "simple-text" "Hello, World!",
      goldenTestShow "show-list" [1, 2, 3 :: Int],
      goldenTestBinary "binary-data" "raw bytes"
    ]
