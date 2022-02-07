module BoolSem where
import Data.Maybe

-- import BoolSyn  -- import syntax definition
-- import BoolPP

data BExpr = T | F
           | Not BExpr
           | Or BExpr BExpr
           | And BExpr BExpr
           deriving Show

nnt :: BExpr
nnt = Not (Not T)

tonnt :: BExpr
tonnt = Or T nnt

sem :: BExpr -> Bool
sem T         = True
sem F         = False
sem (Not b)   = not (sem b)
sem (Or b b') = sem b || sem b'
sem (And b b') = sem b && sem b'

fonnf = F `Or` Not (Not F)
f = fonnf `Or` fonnf
t = T `Or` fonnf

demorgan :: BExpr -> Maybe BExpr
demorgan (Not (And x y)) = Just ( Or (Not(x)) (Not(y)) )
demorgan (Not (Or x y )) = Just (And (Not(x)) (Not(y)) )
demorgan (_) = Nothing

-- Use fromJust to print
pretty :: BExpr -> String
pretty (T) = "true"
pretty (F) = "false"
pretty (Not a) = "not" ++ "( " ++ pretty a ++ " )"
pretty (Or a b) = pretty a ++ " or " ++ pretty b
pretty (And a b) = pretty a ++ " and " ++ pretty b

prettyBool :: Bool -> String
prettyBool x
  | x == True = "true"
  | otherwise = "false"

prettyEval :: BExpr -> String
prettyEval expr = pretty expr ++ " = " ++ prettyBool (sem expr)


d1 = Not ( And T F )
d2 = Not ( Or F F )
d3 = Not ( Or (Not T) (And T F) )
