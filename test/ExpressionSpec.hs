module ExpressionSpec where


import Test.Hspec
import Expression

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "simple" $ do
        it "test value" $
            compute (Val 6) `shouldBe` 6
        it "test addition" $
            compute (Add (Val 6) (Val 7)) `shouldBe` 13
        it "test substraction" $
            compute (Sub (Val 6) (Val 7)) `shouldBe` -1
        it "test multiplication" $
            compute (Mul (Val 6) (Val 7)) `shouldBe` 42
        it "test division" $
            compute (Div (Val 12) (Val 3)) `shouldBe` 4
        it "test power" $
            compute (Pow (Val 3) (Val 3)) `shouldBe` 27

    describe "complex" $ do
        it "test Multiply one addition and one multiplication" $
            compute (Mul (Add (Val 1) (Val 1)) (Mul (Val 11) (Val 2))) `shouldBe` 44
        it "test Divide one multiplication and one addition " $
            compute (Div (Mul (Val 4) (Val 5)) (Add (Val 2) (Val 2))) `shouldBe` 5

