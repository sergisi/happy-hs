-- |

module Main where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Parser

testSimplify s s' = testCase s (runNormal s' @=? runSimplify s)

simplificationTests = testGroup
  "Logic Simplification tests"
  [ testSimplify "((A))"                   "A"
  , testSimplify "((A o B))"               "A o B"
  , testSimplify "((A)) i B"               "A i B"
  , testSimplify "((A i B)) i C"           "A i B i C"
  , testSimplify "!(( A o B ) i !(C i D))" "!A i !B o C i D"
  ]

implicationTests = testGroup
  "Implication tests"
  [ testSimplify "A -> B"       "!A o B"
  , testSimplify "A <-> B"      "(!A o B) i (!B o A)"
  , testSimplify "(A i B) -> C" "!A o !B o C"
  ]


tests = testGroup "All tests" [simplificationTests, implicationTests]

main = defaultMain tests
