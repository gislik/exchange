import Test.QuickCheck 
import Test.Hspec
import Exchange

main :: IO ()
main = hspec $ do
  describe "amount" $ do
    it "should be printable" $ do
      show (Amount 3) `shouldBe` "Amount 3.0"
      show (Amount 3.0) `shouldBe` "Amount 3.0"
      show (Amount 2.9) `shouldBe` "Amount 2.9"

    it "should be instances of num" $ property $ do
      \x y -> do
        (Amount x) + (Amount y) `shouldBe` (Amount $ x+y)
        (Amount x) - (Amount y) `shouldBe` (Amount $ x-y)

    it "should be instances of eq" $ forAll (chooseAny `suchThat` \(x, y) -> x /= y) $ do
      \(x, y) -> do
        (Amount x) == (Amount x) `shouldBe` True
        (Amount x) == (Amount 2) `shouldBe` False

  describe "order" $ do
    it "should have an asset" $ do
        assetOf (Order Bid BTC (Time 0) (Amount 0) (Price 0)) `shouldBe` BTC
        assetOf (Order Bid ETH (Time 0) (Amount 0) (Price 0)) `shouldBe` ETH
        assetOf (Order Ask BTC (Time 0) (Amount 0) (Price 0)) `shouldBe` BTC
        assetOf (Order Ask ETH (Time 0) (Amount 0) (Price 0)) `shouldBe` ETH
        
        
