{-# LANGUAGE InstanceSigs #-}

module Main where

    import Prelude hiding (Left, Right)

    main :: IO ()
    main = do
      putStrLn "hello world"
    
    -- Problem 1 --
    
    -- MyEither used to distinguish from Prelude.Either
    data MyEither a b = Left a | Right b
    
    instance Functor (MyEither e) where
        fmap :: (a -> b) -> MyEither e a -> MyEither e b
        fmap f (Left e) = Left e
        fmap f (Right a) = Right $ f a
    
    -- Problem 2 --
    data Tree a = Node a [Tree a]
    
    instance Functor Tree where
        fmap :: (a -> b) -> Tree a -> Tree b
        fmap f (Node a xs) = Node (f a) (fmap (fmap f) xs)
    
    -- Problem 3 --

    -- The composition of two functors is also a functor
    -- We saw this in Problem 2 with the functors for Tree and List

    -- :t fmap . fmap ::(Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)

    newtype Compose f g a = Compose { getCompose :: f (g a) }

    instance (Functor f, Functor g) => Functor (Compose f g) where
        fmap :: (a -> b) -> Compose f g a -> Compose f g b
        fmap f (Compose x) = Compose (fmap (fmap f) x)