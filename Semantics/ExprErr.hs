--
-- Semantics of arithmetic expressions with potential errors
--
module ExprSem where
import Data.Maybe

data Expr = N Int
          | Plus Expr Expr
          | Neg Expr
          | Mult Expr Expr
          | Div Expr Expr
          deriving Show

-- Arithmetic expressions are evaluated to Int.
-- To account for errors, the semantic domain is
-- defined as Maybe Int.
--
type D = Maybe Int


-- The semantics is defined as a function that maps
-- expressions of type Expr to D. Normal results (i.e.
-- integers) are wrapped by the Just constructor to
-- be of type D. Just 'injects' integers into the
-- Maybe type. Error values are represented by the
-- constructor Nothing.
--
-- sem :: Expr -> D
sem :: Expr -> Maybe Int
sem (N i)       = Just i
sem (Neg e)     = case sem e of
                    Just i  -> Just (-i)
                    _       -> Nothing
sem (Plus e e') = case (sem e,sem e') of
                    (Just i,Just j) -> Just (i+j)
                    _               -> Nothing
sem (Mult e e') = case (sem e,sem e') of
                    (Just i, Just j) -> Just (i*j)
                    _                -> Nothing

sem (Div e e')  =
     case (sem e,sem e') of
          (Just i,Just j) -> if j/=0 then Just (i `div` j)
                                     else Nothing
          _               -> Nothing


pretty :: Expr -> String
pretty (N i) = show i
pretty (Plus a b) = "("++pretty a ++ " + " ++ pretty b++")"
pretty (Mult a b) ="("++ pretty a ++ " * " ++ pretty b++")"
pretty (Div a b) = "("++pretty a ++ " div " ++ pretty b++")"
pretty (Neg a ) = " -( " ++ pretty a ++ " )"

prettyExpr :: Maybe Int -> String
prettyExpr a = case a of
          (Just i) -> show i 
          Nothing  -> " Error "

prettyEval :: Expr -> String
prettyEval expr = pretty expr ++ " = " ++ prettyExpr (sem expr)

-- examples
--
e1 = Plus (N 3) (N 4)

e2 = Div (N 21) e1

e3 = Div e2 (Plus e1 (Neg (N 7)))

results = map sem [e1,e2,e3,Plus e2 e3]