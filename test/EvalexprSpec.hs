module EvalexprSpec where

import Test.Hspec
import Expression
import ExprParser

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "simple" $ do
        it "test parse addition" $
            evalExpr "3+2" `shouldBe` Just 5
        it "test parse addition" $
            evalExpr "+3+2+5" `shouldBe` Just 10
        it "test parse multiplication" $
            evalExpr "3*3" `shouldBe` Just 9
        it "test parse expression" $
            evalExpr "3*(3+3)" `shouldBe` Just 18
        it "test parse expression" $
            evalExpr "3+4*3" `shouldBe` Just 15
        it "test parse expression" $
            evalExpr "4*(3+(3*(4+1)))" `shouldBe` Just 72
