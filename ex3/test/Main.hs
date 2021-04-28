-- |

module Main where

import Test.HUnit
import Parser
import qualified System.Exit as Exit
import Control.Monad (when)

testSimplify s s' = TestLabel s (withoutSimplify s' ~=? withSimplify s)

--Ficar bastants 
tests = TestList
  [ testSimplify "((2))" "2"
  , testSimplify "(2 * 3) + 2" "2*3+2"
  , testSimplify "(2 * 3) div (4 div 2)" "2 * 3 div (4 div 2)"
  , testSimplify "(2 * 3) mod (4 mod 2)" "2 * 3 mod (4 mod 2)"
  , testSimplify "((((2+(2*3)) * 3) mod (4 mod 2)*3))" "(2+2*3) * 3 mod (4 mod 2) * 3"
  ]


main = do
  counts <- runTestTT tests
  putStrLn $ showCounts counts
  when (failures counts > 0) Exit.exitFailure
