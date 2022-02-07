--Exercise 1

type Prog = [Cmd]

type Stack = [Either Bool Int]

data Cmd 
    = LDI Int
    | LDB Bool
    | LEQ
    | ADD
    | MULT
    | DUP
    | IFELSE Prog Prog
    deriving Show

run :: Prog -> Stack -> Maybe Stack
run [] [] = Nothing
run []  x = Just x

run (x:xs) y = semCmd x y >>= run xs


semCmd :: Cmd -> Stack -> Maybe Stack
semCmd (LDI n) x = Just ([Right n]++x)
semCmd (LDB b) x = Just ([Left b]++x)

semCmd ADD [] = Nothing
semCmd ADD (Right x:[]) = Nothing
semCmd ADD (Right x:Right y:xs) = Just (([Right(x+y)])++xs)
semCmd ADD (_) =Nothing
semCmd LEQ (Right x: Right y :xs) = Just (([Left (x <= y)])++xs)
semCmd MULT [] = Nothing
semCmd MULT (Right x:[]) = Nothing
semCmd MULT (Right x:Right y:xs) = Just (([Right(x*y)])++xs)
semCmd MULT (_) = Nothing

semCmd (IFELSE [] _ ) (Left True: s) = Just s 
semCmd (IFELSE p1 _ ) (Left True: s) = run p1 s
semCmd (IFELSE _ [] ) (Left False: s) = Just s  
semCmd (IFELSE _ p2 ) (Left False: s) = run p2 s

semCmd DUP [] = Nothing
semCmd DUP (x:xs) = Just ([x]++[x]++xs)
semCmd _ _ = Nothing

--Test Cases
test_stack :: Stack
test_stack = []

stack1 :: Stack
stack1 = [Right 1,Right 3, Right 5, Right 7, Right 9]
stack2 :: Stack
stack2 = [Left True, Right 3]
test1 = [LDI 3,DUP,ADD,DUP,MULT] 
test2 = [LDB True, DUP, IFELSE [LDI 1][LDI 0]]
test3 = [LEQ]
test4 = [ADD, ADD, MULT, DUP]
test5 = [LEQ, IFELSE [] [], LDI 9]
test6 = [LDI 5, LDI 2, LEQ, IFELSE [LDI 10, DUP] [], ADD]
test7 = [LDI 5, LDI 7, LEQ, IFELSE [LDI 10, DUP] [LDI 20, DUP], ADD]
test8 = [LDI 1, LDI 2, LDI 3, LDI 4, LDI 5, ADD, ADD, ADD, ADD]