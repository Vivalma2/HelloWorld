module Ejer7 where
concat' :: [[a]] -> [a]
concat' (x:[]) = x 
concat' (x:xt) = x ++ concat' xt