module Expression 
    ( Expr(..)
    , compute
    ) where

data Expr = Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | Val Float
    deriving (Eq, Show)

compute :: Expr -> Float
compute (Add a b) = operation a b (+)
compute (Sub a b) = operation a b (-)
compute (Mul a b) = operation a b (*)
compute (Div a b) = operation a b (/)
compute (Pow a b) = operation a b (**)
compute (Val a) = a

operation :: Expr -> Expr -> (Float -> Float -> Float) -> Float
operation expr1 expr2 op = compute expr1 `op` compute expr2
