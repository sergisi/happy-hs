-- | Module where it contains the Parser data, as it will need GADTS
module ParserData (Exp(..), implicate)where

import AlexUserState

type Val = Either Double Int

data Exp = TConj Exp Exp
  | TDisj Exp Exp
  | TNeg Exp
  | TVal Char
  | TBrack Exp
  deriving (Read, Eq, Ord)

instance Show Exp where
  show x = case x of
    TConj  exp exp' -> show exp ++ " ∧ " ++ show exp'
    TDisj  exp exp' -> show exp ++ " ∨ " ++ show exp'
    TBrack exp      -> '(': show exp ++ ")"
    TNeg   exp      -> '¬': show exp
    TVal   x        -> [x]


{-| Implicates both expressions:
A -> B = !(A) o (B)

>>> implicate (TVal 'A') (TVal 'B')
-}
implicate :: Exp -> Exp -> Exp
implicate a b =  TDisj (TNeg (TBrack a)) (TBrack b)
