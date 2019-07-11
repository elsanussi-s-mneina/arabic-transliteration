module Main (main) where

import Test.Hspec

import BuckwalterTests
import ISO232Tests

main :: IO ()
main =
  do
  hspec buckwalterSpecs
  hspec iso232Specs
