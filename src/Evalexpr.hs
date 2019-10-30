module Evalexpr where

import           Data.Maybe
import           Expression
import           ExprParser
import           ParseUtils
import           Text.ParserCombinators.ReadP

evalExpr :: String -> Maybe Float
evalExpr s
    | isNothing p = Nothing
    | otherwise = compute <$> p
  where
    p = parseExpr (filter (/= ' ') s)
