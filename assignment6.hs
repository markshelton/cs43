{-# LANGUAGE GADTs #-}

module Main where

main :: IO ()
main = do
  putStrLn "hello world"

data Expr a where
    I       :: Int  -> Expr Int
    B       :: Bool -> Expr Bool
    O       :: Ordering -> Expr Ordering
    Add     :: Expr Int -> Expr Int -> Expr Int
    Mul     :: Expr Int -> Expr Int -> Expr Int
    Eq      :: Eq a => Expr a -> Expr a -> Expr Bool
    Comp    :: Ord a => Expr a -> Expr a -> Expr Ordering
    If      :: Bool -> Expr a -> Expr a -> Expr a
    Pair    :: Expr a -> Expr b -> Expr (a,b)
    Fst     :: Expr (a,b) -> Expr a
    Snd     :: Expr (a,b) -> Expr b 
    Succ    :: Num a => Expr a -> Expr a
    Pred    :: Num a => Expr a -> Expr a
    Not     :: Expr Bool -> Expr Bool

eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (O o) = o
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq  e1 e2) = eval e1 == eval e2
eval (Comp e1 e2) = compare (eval e1) (eval e2)
eval (If p e1 e2) = if p then eval e1 else eval e2
eval (Pair e1 e2) = (eval e1, eval e2)
eval (Fst t) = fst (eval t)
eval (Snd t) = snd (eval t)
eval (Succ n) = eval n + 1
eval (Pred n) = eval n - 1
eval (Not b) = not (eval b)