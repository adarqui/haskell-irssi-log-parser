{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text        as T

import           Data.Time
import           Data.Time.Format

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
        message "01:2 <@adarqui> yo !" `compare` Nothing @?= EQ,
    testCase
      "IRSSI Timestamp" $
        irssiTimestampToUTC (T.pack "Fri Mar 04 09:10:30 2011") `compare` (read "2011-03-04 09:10:30 UTC" :: UTCTime) @?= EQ
  ]
