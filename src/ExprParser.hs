module ExprParser where

import           Control.Applicative
import           Data.Char
import           Debug.Trace
import           Expression
import           Text.ParserCombinators.ReadP

(<++>) a b = (++) <$> a <*> b

(<:>) a b = (:) <$> a <*> b

number = munch1 isDigit

plus = char '+' *> number

minus = char '-' <:> number

integer = plus <|> minus <|> number

decimal = option "" $ char '.' <:> number

parseVal :: ReadP Expr
parseVal = Val . rd <$> (integer <++> decimal)
  where
    rd = read :: String -> Float

parseExpr :: String -> Maybe Expr
parseExpr s
    | null a = Nothing
    | otherwise = Just $ fst $ last a
  where
    a = readP_to_S parseVal s
