-- | Module where it contains the Parser data, as it will need GADTS
module ParserData (Exp (..), simplify, eval) where

import AlexUserState
import Lexer

type Val = Either Double Int

data Exp
  = TSum Exp Exp
  | TMinus Exp Exp
  | TMult Exp Exp
  | TDiv Exp Exp
  | TMod Exp Exp
  | TVal Int
  | TBrack Exp
  deriving (Read, Eq, Ord)

instance Show Exp where
  show x = case x of
    TSum exp exp' -> show exp ++ " + " ++ show exp'
    TMinus exp exp' -> show exp ++ " - " ++ show exp'
    TDiv exp exp' -> show exp ++ " div " ++ show exp'
    TMult exp exp' -> show exp ++ " * " ++ show exp'
    TMod exp exp' -> show exp ++ " mod " ++ show exp'
    TBrack exp -> '(' : show exp ++ ")"
    TVal x -> show x

eval :: Exp -> Int
eval e = case e of
  TSum ex ex' -> eval ex + eval ex'
  TMinus ex ex' -> eval ex - eval ex'
  TDiv ex ex' -> let x = eval ex' in if x /= 0 then eval ex `div` x else eval ex
  TMod ex ex' -> let x = eval ex' in if x /= 0 then eval ex `mod` x else eval ex
  TMult ex ex' -> eval ex * eval ex'
  TBrack ex -> eval ex
  TVal x -> x

data WasExp
  = WasSum
  | WasMinus
  | WasMult
  | WasDiv
  | WasMod
  | WasVal
  | WasNone
  | WasBrack
  | WasModOrDivRight
  deriving (Read, Eq, Ord, Show, Enum)

simplify :: Exp -> Exp
simplify = simplify' WasNone . removeRedundant

getWasExp :: Exp -> WasExp
getWasExp e = case e of
  TSum _ _ -> WasSum
  TMinus _ _ -> WasMinus
  TMult _ _ -> WasMult
  TDiv _ _ -> WasDiv
  TMod _ _ -> WasMod
  TVal _ -> WasVal
  TBrack _ -> WasBrack

-- | Simplifies the expression removing unnecessary brackets
-- ===Example
-- >>> simplify' $ TBrack (TVal 3)
-- TVal 3
simplify' :: WasExp -> Exp -> Exp
simplify' w x = case x of
  TMinus exp exp' -> TMinus (simplify' WasSum exp) $ simplify' WasMult exp'
  TMult exp exp' -> TMult (simplify' WasMult exp) $ simplify' WasMult exp'
  TSum exp exp' -> TSum (simplify' WasSum exp) $ simplify' WasSum exp'
  TDiv exp exp' -> TDiv (simplify' WasMult exp) $ simplify' WasModOrDivRight exp'
  TMod exp exp' -> TMod (simplify' WasMult exp) $ simplify' WasModOrDivRight exp'
  TBrack exp -> case removeRedundant exp of
    TVal x -> TVal x
    exp -> let exp' = simplify' WasBrack exp; w' = getWasExp exp' in
      if w == WasNone || w' >= w
      then exp'
      else TBrack exp'
  TVal x -> TVal x

removeRedundant :: Exp -> Exp
removeRedundant (TBrack e) = removeRedundant e
removeRedundant x = x
