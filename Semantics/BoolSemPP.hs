module BoolSem where

-- import BoolSyn  -- import syntax definition
-- import BoolPP

data BExpr = T | F
           | Not BExpr
           | Or BExpr BExpr
           deriving Show

-- data BExpr = C Bool | ...

nnt :: BExpr
nnt = Not (Not T)

tonnt :: BExpr
tonnt = Or T nnt

sem :: BExpr -> Bool
sem T         = True
sem F         = False
sem (Not b)   = not (sem b)
sem (Or b b') = sem b || sem b'
-- sem (Or b b') | sem b     = True
--               | otherwise = sem b'
-- sem (Or b b') = case sem b of
--				     True  -> True
--                   False -> sem b'


fonnf = F `Or` Not (Not F)


f = fonnf `Or` fonnf
t = T `Or` fonnf

pretty :: BExpr -> String
pretty (T) = "true"
pretty (F) = "false"
pretty (Not a) = "not" ++ " " ++ pretty a
pretty (Or a b) = pretty a ++ " or " ++ pretty b

prettyBool :: Bool -> String
prettyBool x
  | x == True = "true"
  | otherwise = "false"

prettyEval :: BExpr -> String
prettyEval expr = pretty expr ++ " = " ++ prettyBool (sem expr)