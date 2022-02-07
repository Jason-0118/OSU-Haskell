import HW2types
import Data.List 

myGraph :: Graph
myGraph = [(1,2), (2,3), (1,4), (4,1), (5, 6), (2,6)]

-- hasPath directed graph
hasPath :: Graph -> Node -> Node -> Bool
hasPath [] x y = x == y 
hasPath xs x y 
  | x == y = True
  | otherwise = 
    let xs' = [ (n,m) | (n,m) <- xs, n /= x ] in 
    or [ hasPath xs' m y | (n,m) <- xs, n == x ]

completeG :: Int -> Graph
completeG n = [ (x,y) | x <- [1..n], y<-[1..n] ]

vertexList :: Graph -> [Node]
vertexList [] = []
vertexList xs = nub ([ x | (x,y) <- xs] `union` [ y | (x,y) <- xs])


-- Strongly Connected
stcon :: Graph -> Bool
stcon [] = False
stcon xs = let ys = vertexList xs in
          and [ hasPath xs x y | x <- ys, y <- ys, x /= y ]

-- Trees

data Tree = Node Int Tree Tree | Leaf 
  deriving Show

singleton :: Int -> Tree
singleton x = Node x Leaf Leaf

treeInsert :: Int -> Tree -> Tree
treeInsert x Leaf = singleton x 
treeInsert x (Node n left right)
  | x == n = Node n left right
  | x < n  = Node n (treeInsert x left) right
  | x > n  = Node n left (treeInsert x right)


myTree :: Tree
myTree = Node 5 (Node 3(Node 1 Leaf Leaf)(Node 4 Leaf Leaf))
    (Node 7 (Node 6 Leaf Leaf) (Node 9 Leaf Leaf))

-- Use a foldr to build a tree from a List
numbers = [5, 7, 2, 9, 11, 6]
numberTree = foldr treeInsert Leaf numbers

buildTree :: [Int] -> Tree
buildTree xs = foldr treeInsert Leaf xs

mergeTrees :: Tree -> Tree -> Tree
mergeTrees xs ys = let zs = inorder (ys) in
              foldr treeInsert xs zs

inorder :: Tree -> [Int]
inorder Leaf = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

occurs :: Int -> Tree -> Bool
occurs m (Leaf) = False
occurs m (Node n l r) 
  | m==n = True
  | m<n  = occurs m l
  | m>n  = occurs m r

data Pet = Cat | Dog | Bird
          deriving Show
tigger :: Pet
tigger = Cat
