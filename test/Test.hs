module Main (main) where

import Test.Hspec

import BuckwalterTests

main :: IO ()
main =
  do
  hspec buckwalterSpecs
