module EvalexprSpec where


import Test.Hspec
import Evalexpr

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "simple" $ do
        it "test addition" $
            evalExpr (Add (Value 6) (Value 7)) `shouldBe` 13
        it "test substraction" $
            evalExpr (Sub (Value 6) (Value 7)) `shouldBe` -1
        it "test multiplication" $
            evalExpr (Mul (Value 6) (Value 7)) `shouldBe` 42
        it "test division" $
            evalExpr (Div (Value 12) (Value 3)) `shouldBe` 4
        it "test power" $
            evalExpr (Pow (Value 3) (Value 3)) `shouldBe` 27

    describe "complex" $ do
        it "test Multiply one addition and one multiplication" $
            evalExpr (Mul (Add (Value 1) (Value 1)) (Mul (Value 11) (Value 2))) `shouldBe` 44
        it "test Divide one multiplication and one addition " $
            evalExpr (Div (Mul (Value 4) (Value 5)) (Add (Value 2) (Value 2))) `shouldBe` 5

