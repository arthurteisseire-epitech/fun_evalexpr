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
        it "test parse multiple substraction" $
            parseExpr "3-2-3" `shouldBe` Just (Sub (Sub (Val 3) (Val 2)) (Val 3))
        it "test parse multiple division" $
            parseExpr "3/2/3" `shouldBe` Just (Div (Div (Val 3) (Val 2)) (Val 3))
