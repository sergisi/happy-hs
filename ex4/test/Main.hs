-- | Hello

module Main where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Parser
import           ParserData


testSimplify s exp = testCase s (exp @=? runExpression s)

allTests = testGroup
  "Regex tests"
  [ testSimplify "a . b" $ Right
    [ State 0
            3
            [Action 1 2 Nothing, Action 0 1 (Just 'a'), Action 2 3 $ Just 'b']
    ]
  , testSimplify "a | b" $ Right
    [ State
        4
        5
        [ Action 4 0 Nothing
        , Action 4 2 Nothing
        , Action 1 5 Nothing
        , Action 3 5 Nothing
        , Action 0 1 $ Just 'a'
        , Action 2 3 $ Just 'b'
        ]
    ]
  , testSimplify "a *" $ Right
    [State 0 1 [Action 1 0 Nothing, Action 0 1 Nothing, Action 0 1 $ Just 'a']]
  , testSimplify "a ?"
    $ Right [State 0 1 [Action 0 1 Nothing, Action 0 1 (Just 'a')]]
  , testSimplify "(a | b)* . c?" $ Right
    [ State
        { _initialState = 4
        , _endState = 7
        , _actions = [ Action { _initial = 5, _end = 6, _symbol = Nothing }
                     , Action { _initial = 5, _end = 4, _symbol = Nothing }
                     , Action { _initial = 4, _end = 5, _symbol = Nothing }
                     , Action { _initial = 4, _end = 0, _symbol = Nothing }
                     , Action { _initial = 4, _end = 2, _symbol = Nothing }
                     , Action { _initial = 1, _end = 5, _symbol = Nothing }
                     , Action { _initial = 3, _end = 5, _symbol = Nothing }
                     , Action { _initial = 0, _end = 1, _symbol = Just 'a' }
                     , Action { _initial = 2, _end = 3, _symbol = Just 'b' }
                     , Action { _initial = 6, _end = 7, _symbol = Nothing }
                     , Action { _initial = 6, _end = 7, _symbol = Just 'c' }
                     ]
        }
    ]
  ]


tests = testGroup "Tests" [allTests]

main = defaultMain tests
