{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty
import           Test.Tasty.HUnit

import           Parser.Irssi.Log

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ unitTests ]

unitTests = testGroup "UnitTests"
  [
    testCase
      "Log parsing: message (PASS)" $
        message "01:02 <@adarqui> yo !" `compare` (Just $ Message (01,02) "@" "adarqui" "yo !") @?= EQ,
    testCase
      "Log parsing: message (FAIL)" $
        message "01:2 <@adarqui> yo !" `compare` Nothing @?= EQ
  ]
