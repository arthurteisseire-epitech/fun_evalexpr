module EvalexprSpec where


import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "absolute" $ do
        it "should success" $
            "toto" `shouldBe` "toto"
        it "should success too" $
            1 `shouldBe` 1
