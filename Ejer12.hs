module Ejer12 where
filter' :: (a -> Bool) -> [a] -> [a]
filter' bol xs = [y| y <- xs , bol]

map' :: (a -> b) -> [a] -> [b]
map' fun xs = [fun y | y <- xs]