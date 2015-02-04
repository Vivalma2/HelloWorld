module Ejer10 where
iSort :: [Int] -> [Int]
ins :: Int -> [Int] -> [Int]

iSort [] = []
iSort (x:xs) = ins x (iSort xs)
ins y [] = [y]
ins y (x:xs)
		|y>x = x:ins y xs	
		|otherwise = y:x:xs