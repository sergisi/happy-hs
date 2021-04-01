-- | Module where it contains the Parser data, as it will need GADTS
{-# LANGUAGE GADTs #-}


module ParserData where
import qualified Data.Map.Strict as Map


data Exp a where
  TSum :: (Num a) => Exp a -> Exp a -> Exp a
  TMinus :: (Num a) => Exp a -> Exp a -> Exp a
  TMult :: (Num a) => Exp a -> Exp a -> Exp a
  TDiv :: Exp Int -> Exp Int -> Exp Int
  TVal :: Int -> Exp Int
  TBrack :: Exp a -> Exp a
  TRealAssign :: Char -> Exp Double -> Exp Double
  TIntAssign :: Char ->  Exp Int -> Exp Int
  TRealGet :: Char -> Exp Double
  TIntGet :: Char -> Exp Int
-- Cannot derive from GADTs

type State = (Map.Map Char Integer, Map.Map Char Double)

{-| Evaluates GADT mantaining the state

* Examples of usage
>>> eval (Map.empty, Map.empty) (TSum (TVal 2) $ TVal 3)
((fromList [],fromList []),5)

>>> eval (Map.empty, Map.empty) (TVal 2)
((fromList [],fromList []),2)

-}
eval :: State -- ^ State of the evaluation. Later it will use Alex Monad instead
  -> Exp a -- ^ Expresion to evaluate
  -> (State, a)
eval st (TSum a' b') = (st, a + b)
  where (_, a) = eval st a'
        (_, b) = eval st b'
eval st (TMult a' b') = (st, a * b)
  where (_, a) = eval st a'
        (_, b) = eval st b'
eval st (TDiv a' b') = (st, a `div` b)
  where (_, a) = eval st a'
        (_, b) = eval st b'
eval st (TMinus a' b') = (st, a - b)
  where (_, a) = eval st a'
        (_, b) = eval st b'
eval st (TBrack a) = eval st a
eval st (TVal a) = (st, a)
