module ParserSpec where


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
        it "test parsing value" $
            parseExpr "12.0" `shouldBe` Just (Val 12)
        it "test parsing value" $
            parseExpr "21.2" `shouldBe` Just (Val 21.2)
        it "test parsing value" $
            parseExpr "+21.2" `shouldBe` Just (Val 21.2)
        it "test parsing value" $
            parseExpr "-21.2" `shouldBe` Just (Val (-21.2))
