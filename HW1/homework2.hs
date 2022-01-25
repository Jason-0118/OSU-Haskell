--Xin Zhang, CS38, 1/25/2022

import HW2types

---------------- Answers for Exercise 1 ----------------
--(a)
ins :: Eq a => a -> Bag a -> Bag a
ins e [] = [(e, 1)]
ins e((a,b):xs) | a == e = (a, b+1):xs
                | otherwise = (a,b):ins e xs

-- (b)
del :: Eq a => a -> Bag a -> Bag a
del e [] = []
del e ((a,b):xs)  | (a == e) && (b == 1) = xs
                    | a == e = (a, b - 1):xs
                    | otherwise = (a,b):del e xs

-- (c)
xs = [7,3,8,7,3,2,7,5]
bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = ins x (bag xs)

--(d)
check (a,b) []     = False
check (a,b) ((c,d):ys)  | (a == c) && (b <= d) = True
                        | otherwise = check (a,b) ys
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag xs [] = False
subbag [] ys = True
subbag (x:xs) ys = if length xs+1 > length ys then False else check x ys && subbag xs ys

-- (e)
isSet :: Eq a => Bag a -> Bool
isSet [] = False
isSet [(_,1)] = True
isSet ((a,b):xs) =  if (b == 1) && (isSet xs) then True else False

-- (f)
size :: Bag a -> Int
size [] = 0
size ((a,b):xs) = b + size xs

---------------- Answers for Exercise 2 ----------------
g :: Graph
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]
h :: Graph
h = [(1,2),(1,3),(2,1),(3,2),(4,4)]

--(a)
nodes :: Graph -> [Node]
nodes [] = []
nodes ((a,b):xs) = norm (a:nodes xs ++ b:nodes xs)

--(b)
suc :: Node -> Graph -> [Node]
suc e [] = []
suc e ((a,b):xs)    | a == e = b : suc e xs   
                    | otherwise = suc e xs

--(c)
detach :: Node -> Graph -> Graph
detach e [] = []
detach e ((a,b):xs) | (a /= e) && (b /= e) = (a,b): detach e xs
                    | otherwise = detach e xs

--(d)
preC :: Int -> Graph
preC 1 = []
preC e = (preC (e-1) ++ [(e-1,e)])
cyc e = preC e ++ [(e,1)]

---------------- Answers for Exercise 3 ----------------
f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
--(a)
width :: Shape -> Length
width (Pt _) = 0
width (Circle _ l) = l * 2
width (Rect _ l1 l2) = l1

--(b)
bbox :: Shape -> BBox
bbox (Pt p) = (p,p)
bbox (Circle (a,b) l) = ((a-l,b-l),(a+l,b+l))
bbox (Rect (a,b) l1 l2) = ((a,b),(a+l1,b+l2))

--(c)
minX :: Shape -> Number
minX (Pt (a,b)) = a
minX (Circle (a,b) l) = a-l
minX (Rect (a,b) l1 l2) = a

--(d)
addPt :: Point -> Point -> Point
addPt (a,b) (c,d) = (a+c,b+d)
move :: Shape -> Point -> Shape
move (Pt p1) p2 = Pt (addPt p1 p2)
move (Circle p1 l) p2 = Circle (addPt p1 p2) l
move (Rect p1 l1 l2) p2 = Rect (addPt p1 p2) l1 l2

