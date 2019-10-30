module Evalexpr where

import           Data.Char                    (isDigit)
import           Data.Maybe
import           Expression
import           ExprParser
import           ParseUtils
import           Text.ParserCombinators.ReadP

evalExpr :: String -> Maybe Float
evalExpr s = compute <$> (parseStr s >>= parseExpr)

parseStr :: String -> Maybe String
parseStr s
    | isStringValid str = Just str
    | otherwise = Nothing
  where
    str = filter (/= ' ') s

isStringValid :: String -> Bool
isStringValid = all isCharValid

isCharValid :: Char -> Bool
isCharValid c = isDigit c || c `elem` "()^*/+-"
