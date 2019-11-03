module EvalexprSpec where

import Test.Hspec
import Expression
import ExprParser
import Evalexpr

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "error management" $ do
        it "test plus + blank" $
            evalExpr "3 + " `shouldBe` Nothing
        it "test minus + blank" $
            evalExpr "3 - " `shouldBe` Nothing
        it "test product + blank" $
            evalExpr "3 * " `shouldBe` Nothing
        it "test division + blank" $
            evalExpr "3 / " `shouldBe` Nothing
        it "test power + blank" $
            evalExpr "3 ^ " `shouldBe` Nothing
        it "test parenthesis + blank" $
            evalExpr "(3) + " `shouldBe` Nothing
    describe "without spaces" $ do
        it "test parse addition" $
            evalExpr "3-2-3" `shouldBe` Just (-2)
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
    describe "with spaces" $ do
        it "test value" $
            evalExpr " 3" `shouldBe` Just 3
        it "test expression" $
            evalExpr "4 +   3 *2" `shouldBe` Just 10
        it "test expression with parentheses" $
            evalExpr "4 +   3 * ( 2         + 8   ) " `shouldBe` Just 34
    describe "substraction" $ do
        it "test negative value" $
            evalExpr " -3" `shouldBe` Just (-3)
        it "test sub" $
            evalExpr "3-3" `shouldBe` Just 0
        it "test sub and add" $
            evalExpr "3-3+2" `shouldBe` Just 2
        it "test expression with parentheses" $
            evalExpr "4 +   3 * ( 2         - 8   ) " `shouldBe` Just (-14)
    describe "division" $ do
        it "test sub" $
            evalExpr "3/3" `shouldBe` Just 1
        it "test sub2" $
            evalExpr "(8+8)/4*3" `shouldBe` Just 12
    describe "power" $ do
        it "test simple power" $
            evalExpr "2^3" `shouldBe` Just 8
        it "test priority power" $
            evalExpr "2^3+2" `shouldBe` Just 10
        it "test priority power2" $
            evalExpr "2+2^(3+2)" `shouldBe` Just 34
