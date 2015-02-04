module Ejer9 where
selectEvenPos :: [Int] -> [Int]
selectEvenPos [] = []
selectEvenPos (x:xs) pos = if even pos then x:r
						   else r
						   where r = selectEvenPos xs (pos+1)