--HW4
--Xin Zhang
--CS381

type Prog = [Cmd]
type Stack = [Either Bool Int]
data Cmd  
    = LDI Int                   --right
    | LDB Bool                  --left
    | LEQ                       --left
    | ADD                       --right
    | MULT                      --right
    | DUP                       --right/left
    | IFELSE Prog Prog
    deriving Show   

run :: Prog -> Stack -> Maybe Stack
run [] [] = Nothing
run [] s = Just s
run (c:cs) s  = case semCmd c s of 
                Just s' -> run cs s'
                Nothing -> Nothing


semCmd :: Cmd -> Stack -> Maybe Stack
semCmd (LDI n) s = Just ([Right n]++s)
semCmd (LDB b) s = Just ([Left b]++s)

--Add operator
semCmd ADD [] = Nothing
semCmd ADD (Right x:[]) = Nothing
semCmd ADD (Right x:Right y:xs) = Just ([Right(x+y)] ++ xs)
semCmd ADD _ = Nothing
--Multiplication operator
semCmd MULT [] = Nothing
semCmd MULT (Right x:[]) = Nothing
semCmd MULT (Right x:Right y:xs) = Just  ([Right(x*y)]++xs)
semCmd MULT _ = Nothing
--Duplicate function
semCmd DUP [] = Nothing
semCmd DUP (x:xs) = Just ([x]++[x]++xs)
-- <= operator
semCmd LEQ [] = Nothing
semCmd LEQ (Right x:[]) = Nothing
semCmd LEQ (Right x:Right y:xs) = Just ([Left (x<=y)]++xs)
semCmd LEQ _ = Nothing
-- IFELSE statement
semCmd (IFELSE [] _ ) (Left True: ss) = Just ss
semCmd (IFELSE p1 _ ) (Left True: ss) = run p1 ss
semCmd (IFELSE _ [] ) (Left False: ss) = Just ss  
semCmd (IFELSE _ p2 ) (Left False: ss) = run p2 ss


--test
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