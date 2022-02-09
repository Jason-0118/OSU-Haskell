--HW4-part 1
--Xin Zhang
--CS381

type Prog = [Cmd]
data Cmd 
    = LD Int
    | ADD
    | MULT
    | DUP
    deriving Show
type Stack = [Int]
run :: Prog -> Stack -> Maybe Stack
run [] [] = Just []
run [] s = Just s
run (c:cs) s = case semCmd c s of
                Just s' -> run cs s'
                Nothing -> Nothing

semCmd :: Cmd -> Stack -> Maybe Stack 
semCmd (LD n) (s) =  Just (n:s) 

--Add operator
semCmd ADD [] = Nothing
semCmd ADD (x:[]) = Nothing
semCmd ADD (x:y:xs) = Just ([x+y] ++ xs)
semCmd ADD _ = Nothing
--Multiplication operator
semCmd MULT [] = Nothing
semCmd MULT (x:[]) = Nothing
semCmd MULT (x:y:xs) = Just  ([x*y]++xs)
semCmd MULT _ = Nothing
--Duplicate function
semCmd DUP [] = Nothing
semCmd DUP (x:xs) = Just ([x]++[x]++xs)

--
stack1 :: Stack
stack1 = [1, 2, 3, 4, 5] 
test1 = [LD 3,DUP,ADD,DUP,MULT]  
test2 = [LD 3,ADD] 
test3 = [] 
test4 = [ADD, ADD, ADD, ADD] 