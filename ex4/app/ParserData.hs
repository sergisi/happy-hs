{-# LANGUAGE TemplateHaskell #-}

-- | Module where it contains the Parser data, as it will need GADTS
module ParserData where

import           AlexUserState
import qualified Data.Map.Strict               as Map
import           Lens.Micro
import           Lens.Micro.TH
import           Lexer

data Action = Action
  { _initial :: Int,
    _end :: Int,
    _symbol :: Maybe Char
  }
  deriving (Read, Eq, Ord, Show)

makeLenses ''Action

class Repr a where
  repr :: a -> String

instance Repr Int where
  repr = show

instance Repr Action where
  repr a = case a ^. symbol of
    Just c ->
      "[Estat " ++ repr (a ^. initial) ++ ", Simbol " ++ c : "] Go to " ++ repr
        (a ^. end)
    Nothing ->
      "[Estat " ++ repr (a ^. initial) ++ ", Lambda] Go to " ++ repr (a ^. end)

data State = State
  { -- | Initial State of the machine
    _initialState :: Int,
    -- | End State of the machine
    _endState :: Int,
    -- | Actions of the Machine
    _actions :: [Action]
  }
  deriving (Read, Eq, Ord, Show)

makeLenses ''State

instance Repr State where
  repr st =
    unlines
      $  [ ""
         , "Descripció del AF"
         ,
        -- "Estats numerats del " ++ show (st ^. initialState) ++ " al " ++ show (st ^. endState),
           "Estat inicial: " ++ repr (st ^. initialState)
         , "Estat final: " ++ repr (st ^. endState)
         ]
      ++ (map repr $ st ^. actions)

-- | States to evaluate the different actions.
-- It starts at negative so there is no need to change the starting value
-- of the action.
state1 = State (-2) (-1) [Action (-2) (-1) $ Just 'a']

state2 = State (-4) (-3) [Action (-4) (-3) $ Just 'b']

-- | Alternative creation
-- === Example
-- >>> runAlex "" $ alternative state1 state2
-- Right Descripció del AF
-- Estats numerats del 0 al 1
-- [Estat 0, Lambda] Go to -2
-- [Estat 0, Lambda] Go to -4
-- [Estat -1, Lambda] Go to 1
-- [Estat -3, Lambda] Go to 1
-- [Estat -2, Simbol a] Go to -1
-- [Estat -4, Simbol b] Go to -3
alternative :: State -> State -> Alex State
alternative exp1 exp2 = do
  s <- alexGetUserState
  let ini1 = exp1 ^. initialState
  let ini2 = exp2 ^. initialState
  let end1 = exp1 ^. endState
  let end2 = exp2 ^. endState
  let s'   = s + 1
  alexSetUserState $ s + 2
  return $ State
    s
    s'
    (  [ Action s    ini1 Nothing
       , Action s    ini2 Nothing
       , Action end1 s'   Nothing
       , Action end2 s'   Nothing
       ]
    ++ exp1
    ^. actions
    ++ exp2
    ^. actions
    )

-- | Concat of two states
--
-- === Exemple
-- >>> concaten state1 state2
-- Descripció del AF
-- Estats numerats del -2 al -3
-- Estat inicial: -2
-- Estat final: -3
-- [Estat -1, Lambda] Go to -4
-- [Estat -2, Simbol a] Go to -1
-- [Estat -4, Simbol b] Go to -3
concaten :: State -> State -> State
concaten exp1 exp2 = State
  (exp1 ^. initialState)
  (exp2 ^. endState)
  ( Action (exp1 ^. endState) (exp2 ^. initialState) Nothing
  : (exp1 ^. actions ++ exp2 ^. actions)
  )

-- | Exists of a State
--
-- === Exemple
-- >>> exist state1
-- Descripció del AF
-- Estats numerats del -2 al -1
-- Estat inicial: -2
-- Estat final: -1
-- [Estat -2, Lambda] Go to -1
-- [Estat -2, Simbol a] Go to -1
exist :: State -> State
exist exp =
  over actions (Action (exp ^. initialState) (exp ^. endState) Nothing :) exp

-- | Kleen Star of a expression
--
--
-- === Exemple
-- >>> kleen state1
-- Descripció del AF
-- Estats numerats del -2 al -1
-- Estat inicial: -2
-- Estat final: -1
-- [Estat -1, Lambda] Go to -2
-- [Estat -2, Lambda] Go to -1
-- [Estat -2, Simbol a] Go to -1
kleen :: State -> State
kleen = plus . exist

-- | Regex+
--
-- === Exemple
--
-- >>> plus state1
-- Descripció del AF
-- Estats numerats del -2 al -1
-- Estat inicial: -2
-- Estat final: -1
-- [Estat -1, Lambda] Go to -2
-- [Estat -2, Simbol a] Go to -1
plus :: State -> State
plus exp =
  over actions (Action (exp ^. endState) (exp ^. initialState) Nothing :) exp

-- | Creates a State from a Maybe Char.
--
-- === Exemples
-- >>> runAlex "" . stateFromChar $ Just 'a'
-- Right Descripció del AF
-- Estats numerats del 0 al 1
-- Estat inicial: 0
-- Estat final: 1
-- [Estat 0, Simbol a] Go to 1
stateFromChar :: Maybe Char -> Alex State
stateFromChar c = do
  s <- alexGetUserState
  alexSetUserState $ s + 2
  let s' = s + 1
  return $ State s s' [Action s s' c]
