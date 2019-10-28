module ExprParser where

import           Debug.Trace
import           Expression
import           Text.ParserCombinators.ReadP
import           ValueParser

parseExpr :: String -> Maybe Expr
parseExpr s
    | null a = Nothing
    | otherwise = Just $ fst $ last a
  where
    a = readP_to_S parseVal s
