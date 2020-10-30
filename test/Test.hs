module Main (main) where

import LittleTest

import qualified BuckwalterTests

import ISO233_2Tests

main :: IO ()
main = 
  do
  LittleTest.reportOnTests (BuckwalterTests.tests ++ ISO233_2Tests.tests)
