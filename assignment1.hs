-- Assignment 1

-- Problem 1:
-- Implement the map function using a fold. 

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

-- Problem 2:
-- Implement the filter function using a fold. 

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x xs -> if f x then x:xs else xs) []

-- Problem 3:
-- Implement foldl using foldr

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f a list = (foldr construct id list) a
  where construct x r acc = r (f acc x)

-- Problem 4:
-- Write code to compute the smallest positive number that is evenly divisible by all the numbers from 1 to n.
-- Provide an answer for n = 20 => 232,792,560

smallestDivides :: Int -> Int
smallestDivides n = foldr lcm 1 [1..n]

-- Problem 5:
-- Write code to compute the nth prime number. 
-- Provide an answer for n = 10,001 => 104,759

nthPrime :: Int -> Int
nthPrime = (!!) primes

primes :: [Int]
primes = 2 : 3 : filter (isPrime primes) [5, 7 ..]

isPrime :: [Int] -> Int -> Bool
isPrime (p:ps) n
    | p*p > n = True 
    | otherwise = n `rem` p /= 0 && isPrime ps n