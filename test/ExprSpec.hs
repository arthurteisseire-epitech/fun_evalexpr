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
--        it "test parse addition" $
--            parseExpr "+3+2+5" `shouldBe` Just (Add (Val 3) (Add (Val 2) (Val 5)))
        it "test parse multiple substraction" $
            parseExpr "3-2-3" `shouldBe` Just (Sub (Sub (Val 3) (Val 2)) (Val 3))
