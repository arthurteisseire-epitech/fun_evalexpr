module ExprParser where

import Expression
import Text.ParserCombinators.ReadP
import Data.Char
import Debug.Trace

parseVal :: ReadP Expr
parseVal = do
    n <- read <$> munch1 isDigit
    return $ Val n

parseExpr :: String -> Expr
parseExpr s = fst $ head $ readP_to_S parseVal s
