data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show


symmetric :: Tree a -> Tree a
symmetric (Leaf x) = Leaf x
symmetric (Branch izq der) = Branch (symmetric der) (symmetric izq)

listToTree :: [a] -> Tree a
listToTree [x] = Leaf x
listToTree (x:xs) = Branch (Leaf x) (listToTree xs)

treeToList :: Tree a -> [a]
treeToList (Leaf x) = [x]
treeToList (Branch izq der) = treeToList izq ++ treeToList der

data BinTreeInt = Void | Node Int BinTreeInt BinTreeInt deriving Show

insTree :: Int -> BinTreeInt -> BinTreeInt
insTree x Void = Node x Void Void
insTree x a@(Node y izq der)
				|x==y = a
				|x<y = Node y (insTree x izq) der
				|otherwise = Node y izq (insTree x der)
				
				
creaTree :: [Int] -> BinTreeInt
creaTree [] = Void                    --DUDADUDOSA--
creaTree(x:xs) = (insTree x (creaTree xs))

treeElem :: Int -> BinTreeInt -> Bool
treeElem x Void = False
treeElem x (Node y izq der) 
				|x==y = True
				|x<y = treeElem x izq
				|x>y = treeElem x der

dupElem :: BinTreeInt -> BinTreeInt
dupElem Void = Void
dupElem (Node x izq der) = Node (x*2) (dupElem izq) (dupElem der)










