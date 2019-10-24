module Expression 
    ( Expr(..)
    , evalExpr
    ) where

data Expr = Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | Val Float

evalExpr :: Expr -> Float
evalExpr (Add a b) = operation a b (+)
evalExpr (Sub a b) = operation a b (-)
evalExpr (Mul a b) = operation a b (*)
evalExpr (Div a b) = operation a b (/)
evalExpr (Pow a b) = operation a b (**)
evalExpr (Val a) = a

operation :: Expr -> Expr -> (Float -> Float -> Float) -> Float
operation expr1 expr2 func = func (evalExpr expr1) (evalExpr expr2)
