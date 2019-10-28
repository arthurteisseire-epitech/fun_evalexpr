module ValueSpec where


import Test.Hspec
import Expression
import ExprParser

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "simple" $ do
        it "test parsing value error" $
            parseExpr "fsdafsa" `shouldBe` Nothing
        it "test parsing integer value" $
            parseExpr "12.0" `shouldBe` Just (Val 12)
        it "test parsing float value" $
            parseExpr "21.2" `shouldBe` Just (Val 21.2)
        it "test parsing float value with plus" $
            parseExpr "+21.2" `shouldBe` Just (Val 21.2)
        it "test parsing float value with minus" $
            parseExpr "-21.2" `shouldBe` Just (Val (-21.2))
