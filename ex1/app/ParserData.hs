-- | Module where it contains the Parser data, as it will need GADTS
module ParserData (Exp(..), eval)where

import AlexUserState
import qualified Data.Map.Strict as Map
import Lens.Micro
import Lexer

type Val = Either Double Int

data Exp = TSum Exp Exp
  | TMinus Exp Exp
  | TMult Exp Exp
  | TDiv Exp Exp
  | TVal Int
  | TRealVal Double
  | TBrack Exp
  | TRealAssign Char Exp
  | TIntAssign Char Exp
  | TRealGet Char
  | TIntGet Char
  deriving (Show, Read, Eq, Ord)

{-- | Evaluates GADT mantaining the state
-- This functions lets avaluate an Alex (Exp a) to an
-- Alex a using GADTs powerful type system.
--
-- # Examples of usage
-- >>> runAlex "" $ eval . return $ TVal 3
--
-- >>> runAlex "" $ const alexGetUserState =<< (eval $ return $ TIntAssign 'a' $ TSum (TVal 3) (TVal 4))
-}
eval :: Exp -> Alex Val
eval exp =
  case exp of
    TSum ea eb -> do
      a <- eval ea
      b <- eval eb
      liftOperator (+) (+) a b
    TMinus ea eb -> do
      a <- eval ea
      b <- eval eb
      liftOperator (-) (-) a b
    TMult ea eb -> do
      a <- eval ea
      b <- eval eb
      liftOperator (*) (*) a b
    TDiv ea eb -> do
      a <- eval ea
      b <- eval eb
      ifBothRights div a b
    TVal a -> return $ Right a
    TRealVal a -> return $ Left a
    TBrack a -> eval  a
    TRealAssign c expDouble -> do
      s <- alexGetUserState
      b' <- eval expDouble
      case b' of
        Left b -> do
                  alexSetUserState $ over reals (Map.insert c b) s
                  return $ Left b
        Right _ -> do
                   (line, column) <- getLineAndColumn
                   alexError $ "Trying to assign integer at double variable at " ++ show line ++ ':': show column
    TIntAssign c expInt -> do
      s <- alexGetUserState
      b' <- eval expInt
      case b' of
        Right b -> do
                  alexSetUserState $ over integers (Map.insert c b) s
                  return $ Right b
        Left _ -> do
                   (line, column) <- getLineAndColumn
                   alexError $ "Trying to assign double at integer variable at " ++ show line ++ ':': show column
    TRealGet c -> do
      s <- alexGetUserState
      if c `Map.member` (s ^. reals)
        then return . Left $ (s ^. reals) Map.! c
        else do
          (line, column) <- getLineAndColumn
          alexError $ "Character "
                      ++ show c
                      ++ " is not a part of the real definitions  now: "
                      ++ show (s ^. reals)
                      ++ ", at" ++ show line ++ ':': show column
    TIntGet c -> do
      s <- alexGetUserState
      if c `Map.member` (s ^. integers)
        then return . Right $ (s ^. integers) Map.! c
        else do
          (line, column) <- getLineAndColumn
          alexError $ "Character "
                      ++ show c
                      ++ " is not a part of the integers definitions now: "
                      ++ show (s ^. integers)
                      ++ ", at" ++ show line ++ ':': show column

liftOperator :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> Val -> Val -> Alex Val
liftOperator _ f (Left x)  (Left y)  = return . Left $ f x y
liftOperator f _ (Right x) (Right y) = return . Right $ f x y
liftOperator _ _ _ _ = do
  (line, column) <- getLineAndColumn
  alexError $ "Cannot apply operator between doubles and integers. Please cast to either one of them! At "
            ++ show line ++ ':': show column

ifBothRights :: (Int -> Int -> Int) -> Val -> Val -> Alex Val
ifBothRights f (Right x) (Right y) = return . Right $ f x y
ifBothRights _ _ _ = do
  (line, column) <- getLineAndColumn
  alexError $ "Cannot apply operator between doubles and integers. Please cast to either one of them! At "
            ++ show line ++ ':': show column
