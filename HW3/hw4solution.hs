--Team members Saurabh Desai, Pratiksha Aga, Isabelle Bretl, Maxwell Franz, Cody McConnell 


--Exercise -  1
type Prog = [Cmd]

data Cmd = LD Int
           | ADD
           | MULT
           | DUP
           deriving Show

type Stack = [Int]

type D = Stack -> Maybe Stack

 

semCmd :: Cmd -> D
semCmd (LD i) s =  Just (i:s)
semCmd ADD s = case s of 
                (i:j:u) -> Just ((i+j):u)
                _       -> Nothing
semCmd MULT s = case s of 
                (i:j:u) -> Just ((i*j):u)
                _       -> Nothing
semCmd DUP s = case s of 
                (i:u) -> Just (i:i:u)
                _     -> Nothing

sem :: Prog -> D
sem [] s = Just s
sem (o:os) s = case (semCmd o s) of
               Just i -> sem os i
               _      -> Nothing

--arg1 :: Prog
--arg1 = [LD 3,DUP,ADD,DUP,MULT]

test_stack :: Stack
test_stack = []

stack1 :: Stack
stack1 = [1, 3, 5, 3, 7]

test1 = [LD 3,DUP,ADD,DUP,MULT] --Expected output: 3 -> 3 -> 6 -> 6 -> 36
test2 = [LD 3,ADD]
test3 = []
test4 = [ADD, ADD, MULT, DUP]