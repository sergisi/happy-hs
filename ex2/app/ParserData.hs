-- | Module where it contains the Parser data, as it will need GADTS
module ParserData (Exp(..))where

import AlexUserState
import qualified Data.Map.Strict as Map
import Lens.Micro
import Lexer

type Val = Either Double Int

data Exp = TSum Exp Exp
  | TMinus Exp Exp
  | TMult Exp Exp
  | TDiv Exp Exp
  | TMod Exp Exp
  | TVal Int
  | TBrack Exp
  deriving (Read, Eq, Ord)

instance Show Exp where
  show x = case x of
    TSum   exp exp' -> show exp ++ " " ++ show exp' ++ " +"
    TMinus exp exp' -> show exp ++ " " ++ show exp' ++ " -"
    TDiv   exp exp' -> show exp ++ " " ++ show exp' ++ " div"
    TMult  exp exp' -> show exp ++ " " ++ show exp' ++ " *"
    TMod   exp exp' -> show exp ++ " " ++ show exp' ++ " mod"
    TBrack exp      -> show exp
    TVal   x        -> show x
