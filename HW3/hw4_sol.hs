type Prog = [Cmd]

type Stack = [Int]

data Cmd 
    = LD Int
    | ADD
    | MULT
    | DUP
    deriving Show

run :: Prog -> Stack -> Maybe Stack
run [] [] = Just []
run []  s = Just s

--run (x:xs) y = semCmd x y >>= run xs
run (c:cs) s = case semCmd c s of
                Just s' -> run cs s'
                Nothing -> Nothing

semCmd :: Cmd -> Stack -> Maybe Stack
semCmd (LD n) s = Just (n:s)

semCmd ADD [] = Nothing
semCmd ADD (x:[]) = Nothing
semCmd ADD (x:y:xs) = Just (([(x+y)])++xs)

semCmd MULT [] = Nothing
semCmd MULT (x:[]) = Nothing
semCmd MULT (x:y:xs) = Just (([(x*y)])++xs)

semCmd DUP [] = Nothing
semCmd DUP (x:xs) = Just ([x]++[x]++xs)

--Test Cases
test_stack :: Stack
test_stack = []

stack1 :: Stack
stack1 = [1, 2, 3, 4, 5]

test1 = [LD 3,DUP,ADD,DUP,MULT] 
test2 = [LD 3,ADD]
test3 = []
test4 = [ADD, ADD, ADD, ADD]
