module Main where

import           Data.Maybe
import           ExprParser
import           System.Environment
import           Text.Printf

main :: IO ()
main = do
    args <- getArgs
    output $ exec args

output :: Maybe Float -> IO ()
output f
    | isNothing f = putStrLn "Fail"
    | otherwise = printf "%.2f\n" (fromJust f)

exec :: [String] -> Maybe Float
exec s
    | length s /= 1 = Nothing
    | otherwise = evalExpr $ head s
