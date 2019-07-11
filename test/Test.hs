module Main (main) where

import Test.Hspec

import BuckwalterTests
import ISO233_2Tests

main :: IO ()
main =
  do
  hspec buckwalterSpecs
  hspec iso233_2Specs
