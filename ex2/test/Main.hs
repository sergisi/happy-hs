-- |

module Main where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Parser
import           ParserData

import           Control.Monad                  ( when
                                                , liftM
                                                , liftM2
                                                )

testSimplify s exp = testCase s (exp @=? runExpression s)

allTests = testGroup
  "Suffix notation tests"
  [ testSimplify "((2))" $ Right ["2"]
  , testSimplify "2 + 3 * 2" $ Right ["2 3 2 * +"]
  , testSimplify "2 mod 3 * 4 + 5" $ Right ["2 3 mod 4 * 5 +"]
  , testSimplify "3 div 2 mod 4 * 5" $ Right ["3 2 div 4 mod 5 *"]
  , testSimplify "2 * (3 - (4 div 3))" $ Right ["2 3 4 3 div - *"]
  , testSimplify "(10 - 3) div (3 * 5 - 15)" $ Right ["10 3 - 3 5 * 15 - div"]
  , testSimplify "777 + 33 - (10 - (5 * 3))" $ Right ["777 33 + 10 5 3 * - -"]
  ]

tests = testGroup "Tests" [allTests]

main = defaultMain tests
