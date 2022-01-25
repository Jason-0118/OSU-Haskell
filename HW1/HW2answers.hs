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

---------------- test functions ----------------
main:: IO ()
main = 
	do
		let b1 = [(3,2), (4,1),(7,3),(12,5)]
		let b2 = [(3,1), (7,2)]
		let b3 = [(3,1), (4,1),(7,1),(10,1) ]
		putStrLn " b1 : "
		print b1
		putStrLn " b2 : "
		print b2
		putStrLn " b3 : "
		print b3
		putStrLn "Exercise 1  :"
		putStrLn " a) test 1 - ins 5 []  "		
		print ( ins 5 [])
		
		putStrLn " a) test 2 - ins 5 b1 : "			
		print ( ins 5 b1)
			
		putStrLn " a) test 3 -  ins 7 b1 : "	
		
		print ( ins 7 b1)

		-- exercise 1 (b) del 
		
		putStrLn " b) test 1 -  del 5 [] "		
		print ( del 5 [])

		
		putStrLn " b) test 2 - del 5 b1  "	
		print ( del 5 b1)

		
		putStrLn " b) test 3 -  del 7 b1 "	
		print ( del 7 b1)
		
		putStrLn " b) test 4 - del 7 b3 "	
		print ( del 7 b3)

		-- exercise 1 (c) bag
			
		putStrLn " c) test 1 -  bag [1,2]  "	
		print ( bag [1,2])

		putStrLn " c) test 2 -  bag [1,2,1,3,5,2]  "		
		print ( bag [1,2,1,3,5,2])
		-- exercise 1 (d) subbag
		
		putStrLn " d) test 1 -  subbag b1 []  "
		print ( subbag b1[])
		
		putStrLn " d) test 2 -  subbag b1 b2  "
		print ( subbag b1 b2)

		putStrLn " d) test 3 -  subbag b2 b1  "
		print ( subbag b2 b1)	
		
		putStrLn " e) test 1 -  isSet b1 "

		print ( isSet b1)

		putStrLn " e) test 2 -  isSet b2 "

		print ( isSet b2)

		putStrLn " e) test 3 -  isSet b3 "

		print ( isSet b3)

		putStrLn " f) test 1 -  size b1 "

		print ( size b1)
		putStrLn " f) test 2 -  size b2 "

		print ( size b2)

		putStrLn " f) test 3 -  size b3 "

		print ( size b3)
		
		
		let g = [ (1,2), (1,3), (2,3), (2,4),(3,4)]
		let h = [ (1,2), (1,3), (2,1), (3,2), (4,4) ]
		let g2 = [ (1,5), (1,7), (2,5), (7,2) ]
		putStrLn " g : "
		print g
		putStrLn " h : "
		print h
		putStrLn " g2 : "
		print g2
		putStrLn " "
		putStrLn "Exercise 2  :"
		putStrLn " a) test 1 - nodes g"

		print ( nodes g)

		putStrLn " a) test 2 - nodes h"

		print ( nodes h)

		putStrLn " a) test 3 - nodes g2"

		print ( nodes g2)	
		
		putStrLn " b) test 1 - suc 4 g"

		print ( suc 4 g)

		putStrLn " b) test 2 - suc 2 g"

		print ( suc 2 g)
		
		putStrLn " b) test 3 - suc 4 h"
		
		print ( suc 4 h)
		
		putStrLn " c) test 1 - detach 4 g"
		
		print ( detach 4 g)

		putStrLn " c) test 2 - detach 3 g"
		
		print ( detach 3 g)
		putStrLn " c) test 2 - detach 3 h"

		print ( detach 3 h)

		putStrLn " d) test 1 - cyc 4 "

		print ( cyc 4 )

		putStrLn " "
		putStrLn "Exercise 3   :"
		
		let p1 = Pt (4,3)
		let c1 = Circle (5,5) 7
		let r1 = Rect (3,3) 5 3
		putStrLn " p1 : "
		print p1
		putStrLn " c1 : "
		print c1
		putStrLn " r1 : "
		print r1
		putStrLn " a) test 1 - width Point "

		print ( width p1)
		
		putStrLn " a) test 2 - width Circle "
		print ( width c1)

		putStrLn " a) test 3 - width Rect "
		
		print ( width r1)

		putStrLn " b) test 1 - bbox Point "

		print ( bbox p1)

		putStrLn " b) test 2 - bbox Circle "

		print ( bbox c1)

		putStrLn " b) test 3 - bbox Rect "

		print ( bbox r1)

		putStrLn " c) test 1 - minX Point "
		print ( minX p1)

		putStrLn " c) test 2 - minX Circle "
		print ( minX c1)

		putStrLn " c) test 3 - minX Rect "

		print ( minX r1)

		putStrLn " d) test 1 - move Point (3,-2)"

		print ( move p1 (3,-2))

		putStrLn " d) test 2 - move Circle (2,4)"

		print ( move c1 (2,4))	

		putStrLn " d) test 3 - move Rect (-1,3)"
		
		print ( move r1 (-1,3))		