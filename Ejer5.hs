module Ejer5 where
isPrime:: Int -> Bool
primes :: Int -> [Int]
isPrime x = if length[y | y <- [1..x], x`mod`y == 0] <= 2 then True else False
primes n = take n [y | y <- [1..], isPrime y]