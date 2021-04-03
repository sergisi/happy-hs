{-# LANGUAGE GADTs #-}

-- | Module where it contains the Parser data, as it will need GADTS
module ParserData (Exp(..), eval)where

import AlexUserState
import qualified Data.Map.Strict as Map
import Lens.Micro
import Lexer

data Exp a where
  TSum :: (Num a) => Exp a -> Exp a -> Exp a
  TMinus :: (Num a) => Exp a -> Exp a -> Exp a
  TMult :: (Num a) => Exp a -> Exp a -> Exp a
  TDiv :: Exp Int -> Exp Int -> Exp Int
  TVal :: Int -> Exp Int
  TBrack :: Exp a -> Exp a
  TRealAssign :: Char -> Exp Double -> Exp Double
  TIntAssign :: Char -> Exp Int -> Exp Int
  TRealGet :: Char -> Exp Double
  TIntGet :: Char -> Exp Int
-- Cannot derive from GADTs

{-- | Evaluates GADT mantaining the state
-- This functions lets avaluate an Alex (Exp a) to an
-- Alex a using GADTs powerful type system.
--
-- # Examples of usage
-- >>> runAlex "" $ eval . return $ TVal 3
--
-- >>> runAlex "" $ const alexGetUserState =<< (eval $ return $ TIntAssign 'a' $ TSum (TVal 3) (TVal 4))
-}
eval :: Alex (Exp a) -> Alex a
eval alex = do
  exp <- alex
  case exp of
    TSum ea eb -> do
      a <- eval $ return ea
      b <- eval $ return eb
      return $ a + b
    TMinus ea eb -> do
      a <- eval $ return ea
      b <- eval $ return eb
      return $ a - b
    TMult ea eb -> do
      a <- eval $ return ea
      b <- eval $ return eb
      return $ a * b
    TDiv ea eb -> do
      a <- eval $ return ea
      b <- eval $ return eb
      return $ a `div` b
    TVal a -> return a
    TBrack a -> eval $ return a
    TRealAssign c expDouble -> do
      s <- alexGetUserState
      b <- eval $ return expDouble
      alexSetUserState $ over reals (Map.insert c b) s
      return b
    TIntAssign c expInt -> do
      s <- alexGetUserState
      b <- eval $ return expInt
      alexSetUserState $ over integers (Map.insert c b) s
      return b
    TRealGet c -> do
      s <- alexGetUserState
      if c `Map.member` (s ^. reals)
        then do
          (line, column) <- getLineAndColumn
          alexError $ "Character "
                      ++ show c
                      ++ "is not a part of the definitions until now: "
                      ++ show (s ^. reals)
                      ++ ", at" ++ show line ++ ':': show column
        else return $ (s ^. reals) Map.! c
    TIntGet c -> do
      s <- alexGetUserState
      if c `Map.member` (s ^. integers)
        then do
          (line, column) <- getLineAndColumn
          alexError $ "Character "
                      ++ show c
                      ++ "is not a part of the definitions until now: "
                      ++ show (s ^. integers)
                      ++ ", at" ++ show line ++ ':': show column
        else return $ (s ^. integers) Map.! c
