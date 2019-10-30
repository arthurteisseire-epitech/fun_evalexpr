module ExprParser where

import           Control.Applicative          ((<|>))
import           Data.Maybe
import           Expression
import           ParseUtils
import           Text.ParserCombinators.ReadP

parseExpr :: String -> Maybe Expr
parseExpr s
    | null a = Nothing
    | otherwise = Just $ fst $ last a
  where
    a = readP_to_S additive s

additive :: ReadP Expr
additive = do
    expr <- multitive
    additiveSuffix expr

additiveSuffix :: Expr -> ReadP Expr
additiveSuffix expr = (do
    addOrSub <- addition <|> substraction
    m <- multitive
    additiveSuffix $ addOrSub expr m)
    <|> return expr

addition :: ReadP (Expr -> Expr -> Expr)
addition = do
    satisfy (== '+')
    return Add

substraction :: ReadP (Expr -> Expr -> Expr)
substraction = do
    satisfy (== '-')
    return Sub

multitive :: ReadP Expr
multitive = do
    expr <- power
    multitiveSuffix expr

multitiveSuffix :: Expr -> ReadP Expr
multitiveSuffix expr = (do
    mulOrDiv <- multiplication <|> division
    m <- power
    multitiveSuffix $ mulOrDiv expr m)
    <|> return expr

multiplication :: ReadP (Expr -> Expr -> Expr)
multiplication = do
    satisfy (== '*')
    return Mul

division :: ReadP (Expr -> Expr -> Expr)
division = do
    satisfy (== '/')
    return Div

power :: ReadP Expr
power =
    (do p1 <- primary
        satisfy (== '^')
        Pow p1 <$> primary) <|> primary

primary :: ReadP Expr
primary = additiveWithParentheses <|> decimal

additiveWithParentheses :: ReadP Expr
additiveWithParentheses = do
    satisfy (== '(')
    a <- additive
    satisfy (== ')')
    return a

decimal :: ReadP Expr
decimal = Val . rd <$> integer <++> decimalPart <++> e
  where
    rd = read :: String -> Float
    decimalPart = option "" $ char '.' <:> number
    e = option "" $ (char 'e' <|> char 'E') <:> integer
