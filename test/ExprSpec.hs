module ExprSpec where

import Test.Hspec
import Expression
import ExprParser

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "simple" $ do
        it "test parse addition" $
            parseExpr "3+2" `shouldBe` Just (Add (Val 3) (Val 2))

