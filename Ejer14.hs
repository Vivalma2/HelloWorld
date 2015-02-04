module Ejer14 where
type Person = String
type Book = String
type Database =[(Person,Book)]

exampleBase :: Database
exampleBase = [("Alicia","El nombre de la rosa"),("Juan","La hija canibal"),("Pepe","Odesa"),("Alicia","La ciudad de las bestias")]

obtain :: Database -> Person -> [Book]
obtain dBase thisPerson = [book | (person,book) <- dBase, thisPerson == person]

borrow :: Database -> Book -> Person -> Database
borrow dBase book person = (person,book):dBase

return' :: Database -> (Person,Book) -> Database
return' dbase (thisBook,thisPerson) = [(person,book)| (person,book) <- dbase, thisPerson /= person || thisBook /= book]

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

symmetric :: Tree a -> Tree a
symmetric (Leaf x) = Leaf x
symmetric (Branch izq der) = Branch (symmetric der) (symmetric izq)

data BinTreeInt = Void | Node Int BinTreeInt BinTreeInt deriving Show

listToTree :: [a] -> Tree a
listToTree [x] = Leaf x
listToTree (x:xs) = Branch (Leaf x) (listToTree xs)
		
treeToList :: Tree a -> [a]
treeToList (Leaf x) = [x]
treeToList (Branch izq der) = treeToList izq ++ treeToList der


insTree :: Int -> BinTreeInt -> BinTreeInt
insTree x Void = Node x Void Void
insTree x a@(Node y izq der) 
			|x==y = a 
			|x<y = Node y (insTree x izq) der
			|otherwise = Node y izq (insTree x der)
			
--creaTree :: [Int] -> BinTreeInt
--creaTree [] = Void
--creaTree [x] = Nodo x Void Void
--creaTree (x:y:xs)
			
			
			
--treeElem :: Int -> BinTreeInt -> Bool
--treeElem x Void = False
--treeElem x (Nodo y izq der)
--				|x == y = True
--				|x < y = treeElem x izq	
	--			|otherwise = treeElem x der
				
			


















