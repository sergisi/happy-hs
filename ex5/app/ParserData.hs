-- | Module where it contains the Parser data, as it will need GADTS
module ParserData (Exp (..), implicate, expandNegations, simplify) where

import AlexUserState

type Val = Either Double Int

data Exp
  = TConj Exp Exp
  | TDisj Exp Exp
  | TNeg Exp
  | TVal Char
  | TBrack Exp
  deriving (Read, Eq, Ord)

instance Show Exp where
  show x = case x of
    TConj exp exp' -> show exp ++ " ∧ " ++ show exp'
    TDisj exp exp' -> show exp ++ " ∨ " ++ show exp'
    TBrack exp -> '(' : show exp ++ ")"
    TNeg exp -> '¬' : show exp
    TVal x -> [x]

-- | Implicates both expressions:
-- A -> B = !(A) o (B)
--
-- >>> implicate (TVal 'A') (TVal 'B')
implicate :: Exp -> Exp -> Exp
implicate a b = TDisj (TNeg (TBrack a)) (TBrack b)

expandNegations :: Exp -> Exp
expandNegations = expandNegations' False

expandNegations' :: Bool -> Exp -> Exp
expandNegations' False exp = case exp of
  TConj e e' -> TConj (expandNegations' False e) $ expandNegations' False e'
  TDisj e e' -> TDisj (expandNegations' False e) $ expandNegations' False e'
  TBrack e -> TBrack $ expandNegations' False e
  TVal c -> TVal c
  TNeg e -> expandNegations' True e
expandNegations' True exp = case exp of
  TConj e e' -> TBrack $ TDisj (expandNegations' True e) $ expandNegations' True e'
  TDisj e e' -> TConj (expandNegations' True e) $ expandNegations' True e'
  TBrack e -> TBrack $ expandNegations' True e
  TVal c -> TNeg $ TVal c
  TNeg e -> expandNegations' False e

getWasOp :: Exp -> WasOp
getWasOp e = case e of
  TConj _ _ -> WasConj
  TDisj _ _ -> WasDisj
  TNeg _ -> WasNeg
  TBrack _ -> WasBrack
  TVal _ -> WasVal

data WasOp
  = WasDisj
  | WasConj
  | WasNeg
  | WasVal
  | WasNone
  | WasBrack
  deriving (Read, Eq, Ord, Show, Enum)

simplify :: Exp -> Exp
simplify = simplify' WasNone . removeRedundant

simplify' :: WasOp -> Exp -> Exp
simplify' w exp = case exp of
  TConj e e' -> TConj (simplify' WasConj e) $ simplify' WasConj e'
  TDisj e e' -> TDisj (simplify' WasDisj e) $ simplify' WasDisj e'
  TBrack e -> case removeRedundant e of
    TVal x -> TVal x
    exp ->
      let exp' = simplify' WasBrack exp; w' = getWasOp exp'
       in if w == WasNone || w' >= w
            then exp'
            else TBrack exp'
  TVal c -> TVal c
  TNeg e -> TNeg $ simplify' WasNeg e

removeRedundant :: Exp -> Exp
removeRedundant (TBrack e) = removeRedundant e
removeRedundant x = x
