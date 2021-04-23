-- |

module Main where

import Test.HUnit
import Parser
import qualified System.Exit as Exit
import Control.Monad (when)

testSimplify s s' = TestLabel s (runSimplify s ~=? runNormal s')

tests = TestList
  [ testSimplify "((A))" "A"
  , testSimplify "((A o B))" "A o B"
  , testSimplify "((A)) i B" "A i B"
  , testSimplify "((A i B)) i C" "A i B i C"
  , testSimplify "!(( A o B ) i !(C i D))" "!A i !B o C i D"
  ]


main = do
  counts <- runTestTT tests
  putStrLn $ showCounts counts
  when (failures counts > 0) Exit.exitFailure
