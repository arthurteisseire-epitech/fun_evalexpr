module ParserSpec where


import Test.Hspec
import Expression
import ExprParser

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "simple" $ do
        it "test pdarsing value" $
            parseExpr "12," `shouldBe` Val 12
        it "test parsing value" $
            parseExpr "21,2" `shouldBe` Val 21
