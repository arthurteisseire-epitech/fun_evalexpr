module EvalexprSpec where


import Test.Hspec
import Evalexpr

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "simple" $ do
        it "test value" $
            evalExpr (Val 6) `shouldBe` 6
        it "test addition" $
            evalExpr (Add (Val 6) (Val 7)) `shouldBe` 13
        it "test substraction" $
            evalExpr (Sub (Val 6) (Val 7)) `shouldBe` -1
        it "test multiplication" $
            evalExpr (Mul (Val 6) (Val 7)) `shouldBe` 42
        it "test division" $
            evalExpr (Div (Val 12) (Val 3)) `shouldBe` 4
        it "test power" $
            evalExpr (Pow (Val 3) (Val 3)) `shouldBe` 27

    describe "complex" $ do
        it "test Multiply one addition and one multiplication" $
            evalExpr (Mul (Add (Val 1) (Val 1)) (Mul (Val 11) (Val 2))) `shouldBe` 44
        it "test Divide one multiplication and one addition " $
            evalExpr (Div (Mul (Val 4) (Val 5)) (Add (Val 2) (Val 2))) `shouldBe` 5

