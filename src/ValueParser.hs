module ValueParser
    ( parseVal
    ) where

import           Control.Applicative
import           Data.Char
import           Expression
import           Text.ParserCombinators.ReadP

parseVal :: ReadP Expr
parseVal = Val . rd <$> integer <++> decimal
  where
    rd = read :: String -> Float

integer :: ReadP String
integer = plus <|> minus <|> number

decimal :: ReadP String
decimal = option "" $ char '.' <:> number

number :: ReadP String
number = munch1 isDigit

plus :: ReadP String
plus = char '+' *> number

minus :: ReadP String
minus = char '-' <:> number

(<++>) :: ReadP String -> ReadP String -> ReadP String
(<++>) = liftA2 (++)

(<:>) :: ReadP Char -> ReadP String -> ReadP String
(<:>) a b = fmap (:) a <*> b
