--Ejer15
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

numleaves (Leaf x) = 1
numleaves (Branch a b) = numleaves a + numleaves b

--Ejer16
symmetric :: Tree a -> Tree a
symmetric (Leaf x) = Leaf x
symmetric (Branch izq der) = Branch (symmetric der) (symmetric izq)


--Ejer17
listToTree :: [a] -> Tree a
treeToList :: Tree a -> [a]

treeToList (Leaf x) =[x]
treeToList (Branch i d) = (treeToList i)++(treeToList d)

listToTree [x] = Leaf x
listToTree (x:xs) = Branch (Leaf x) (listToTree xs)

--Ejer18 (Arboles pseudoequilibrados)
data BinTreeInt = Void | Node Int BinTreeInt BinTreeInt deriving Show
insTree :: Int -> BinTreeInt -> BinTreeInt

insTree x Void = Node x Void Void
insTree x a@(Node y izq der)
			| x==y = a
			|x<y = Node y (insTree x izq) der
			|otherwise = Node y izq (insTree x der)
			
--Ejer19
creaTree :: [Int] -> BinTreeInt
creaTree [] = Void
creaTree (x:xs) = insTree x (creaTree xs)
	--Otra opcion
	--creaTree' xs = foldr insTree Void

--Ejer20
treeElem :: Int -> BinTreeInt -> Bool
treeElement _ Void = False
treeElem x (Node y izq der)
		| x==y = True
		| x<y = treeElem x izq
		|otherwise = treeElem x der







