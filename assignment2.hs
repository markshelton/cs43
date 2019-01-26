{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Semigroup (stimes, Semigroup, (<>)) 

main :: IO ()
main = do
  putStrLn "hello world"


newtype Product = Product Int deriving (Show)
newtype Sum = Sum Int deriving (Show)

instance Semigroup Sum where
    (Sum x) <> (Sum y) = Sum $ x + y
    
    stimes :: (Integral b) => b -> Sum -> Sum
    stimes n (Sum x) = Sum $ x * fromIntegral n
    
instance Semigroup Product where
    (Product x) <> (Product y) = Product $ x * y
    
    stimes :: (Integral b) => b -> Product -> Product
    stimes n (Product x) = Product $ x ^ fromIntegral n


stimes' :: (Semigroup a, Integral b) => b -> a -> a
stimes' n x
  | n <= 0    = error "positive multiplier expected"
  | n == 1    = x
  | even n    = stimes' (n `div` 2) x <> stimes' (n `div` 2) x
  | otherwise = stimes' (n `div` 2 + 1) x <> stimes' (n `div` 2) x