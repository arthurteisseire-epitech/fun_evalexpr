module Main where

import           Data.Maybe
import           Evalexpr
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
    | otherwise =
        if nb == inf
            then Nothing
            else nb
  where
    nb = evalExpr $ head s
    inf = Just $ 1 / 0

exitWithHelp :: IO ()
exitWithHelp = do
    putStrLn "Usage: ./funEvalExpr 'your_expression'"
    putStrLn "And please, a valid one"
    exitWith (ExitFailure 84)
