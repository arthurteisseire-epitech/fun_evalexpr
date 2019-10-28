module ExprParser where

import           Control.Applicative          ((<|>))
import           Debug.Trace
import           Expression
import           ParseUtils
import           Text.ParserCombinators.ReadP
import           ValueParser

parseExpr :: String -> Maybe Expr
parseExpr s
    | null a = Nothing
    | otherwise = Just $ fst $ last a
  where
    a = readP_to_S parse s

parse :: ReadP Expr
parse = parseAdd <|> parseVal

parseAdd :: ReadP Expr
parseAdd = do
    i1 <- parseVal
    satisfy (== '+')
    i2 <- parse
    return (Add i1 i2)
