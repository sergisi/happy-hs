-- |

module Main where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Parser
import           ParserData

import qualified System.Exit                   as Exit
import           Control.Monad                  ( when
                                                , liftM
                                                , liftM2
                                                )

testSimplify s s' = testCase s (withSimplify s @=? withoutSimplify s')

unitTests = testGroup
  "Unit tests"
  [ testSimplify "((2))"                 "2"
  , testSimplify "(2 * 3) + 2"           "2*3+2"
  , testSimplify "(2 * 3) div (4 div 2)" "2 * 3 div (4 div 2)"
  , testSimplify "(2 * 3) mod (4 mod 2)" "2 * 3 mod (4 mod 2)"
  , testSimplify "2 * (3 + 2)"           "2 * (3 + 2)"
  ]

uglyFunction exp = case withoutSimplify (show exp) of
  Right [x] -> x
  _         -> error "Something that doesn't feel right."

properties = testGroup
  "QuickCheck properties"
  [ testProperty "eval exp === eval (simplify exp)"
      $ \x -> eval x === eval (simplify x)
  ]

instance Arbitrary Exp where
  arbitrary = fmap uglyFunction $ sized tree'
   where
    tree' n
      | n > 0 = oneof
        [ liftM2 TSum   subtree subtree
        , liftM2 TMinus subtree subtree
        , liftM2 TMult  subtree subtree
        , liftM2 TDiv   subtree subtree
        , liftM2 TMod   subtree subtree
        , liftM TVal   arbitrary
        , liftM TBrack subtree
        ]
      | otherwise = liftM TVal arbitrary
      where subtree = tree' (n `div` 2)

tests = testGroup "Tests" [unitTests, properties]

main = defaultMain tests
