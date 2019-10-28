module ValueParser
    ( parseVal
    ) where

import           Control.Applicative
import           Data.Char
import           Expression
import           ParseUtils
import           Text.ParserCombinators.ReadP

parseVal :: ReadP Expr
parseVal = Val . rd <$> integer <++> decimal
  where
    rd = read :: String -> Float

decimal :: ReadP String
decimal = option "" $ char '.' <:> number
