module Main where

import           Data.Maybe
import           ExprParser
import           System.Environment
import           System.Exit
import           Text.Printf

main :: IO ()
main = do
    args <- getArgs
    output $ exec args

output :: Maybe Float -> IO ()
output f
    | isNothing f = exitWithHelp
    | otherwise = printf "%.2f\n" (fromJust f)

exec :: [String] -> Maybe Float
exec s
    | length s /= 1 = Nothing
    | otherwise = evalExpr $ head s

exitWithHelp :: IO ()
exitWithHelp = do
    putStrLn "Usage: ./funEvalExpr 'your_expression'"
    putStrLn "And please, a valid one"
    exitWith (ExitFailure 84)
