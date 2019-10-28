module ExprParser where

import           Control.Applicative
import           Data.Char
import           Debug.Trace
import           Expression
import           Text.ParserCombinators.ReadP

(<++>) :: ReadP String -> ReadP String -> ReadP String
(<++>) a b = fmap (++) a <*> b

(<:>) :: ReadP Char -> ReadP String -> ReadP String
(<:>) a b = fmap (:) a <*> b

number :: ReadP String
number = munch1 isDigit

plus :: ReadP String
plus = char '+' *> number

minus :: ReadP String
minus = char '-' <:> number

integer :: ReadP String
integer = plus <|> minus <|> number

decimal :: ReadP String
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
