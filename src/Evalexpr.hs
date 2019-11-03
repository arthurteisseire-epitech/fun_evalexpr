module Evalexpr where

import           Data.Char                    (isDigit)
import           Data.List
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
isStringValid s =
    all isCharValid s && not (isLastCharAnOperator s) && not (parenthesis `isInfixOf` s) && not (")(" `isInfixOf` s)

isLastCharAnOperator :: String -> Bool
isLastCharAnOperator s = last s `elem` operators

isCharValid :: Char -> Bool
isCharValid c = isDigit c || c `elem` (operators ++ parenthesis)

operators :: String
operators = "^*/+-."

parenthesis :: String
parenthesis = "()"
