module ExprParser where

import           Control.Applicative          ((<|>))
import           Data.Maybe
import           Debug.Trace
import           Expression
import           ParseUtils
import           Text.ParserCombinators.ReadP

evalExpr :: String -> Maybe Float
evalExpr s
    | isNothing p = Nothing
    | otherwise = Just $ compute (fromJust p)
  where
    p = parseExpr (filter (/= ' ') s)

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
multitive = primaryPlusMultitive <|> primary

primaryPlusMultitive :: ReadP Expr
primaryPlusMultitive = do
    p <- primary
    satisfy (== '*')
    m <- multitive
    return $ Mul p m

additiveWithParentheses :: ReadP Expr
additiveWithParentheses = do
    satisfy (== '(')
    a <- additive
    satisfy (== ')')
    return a

primary :: ReadP Expr
primary = additiveWithParentheses <|> decimal

decimal :: ReadP Expr
decimal = Val . rd <$> integer <++> decimalPart
  where
    rd = read :: String -> Float
    decimalPart = option "" $ char '.' <:> number
