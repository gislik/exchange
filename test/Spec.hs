import Test.QuickCheck 
import Test.Hspec
import Exchange

main :: IO ()
main = hspec $ do
  describe "Amount" $ do

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

  describe "Order" $ do

    it "should have an asset" $ do

      assetOf (Order Bid BTC (Time 0) (Amount 0) (Price 0)) `shouldBe` BTC
      assetOf (Order Bid ETH (Time 0) (Amount 0) (Price 0)) `shouldBe` ETH
      assetOf (Order Ask BTC (Time 0) (Amount 0) (Price 0)) `shouldBe` BTC
      assetOf (Order Ask ETH (Time 0) (Amount 0) (Price 0)) `shouldBe` ETH

    it "should have correct sides" $ do

      sideOf (Order Bid BTC (Time 0) (Amount 0) (Price 0)) `shouldBe` Bid
      sideOf (Order Ask BTC (Time 0) (Amount 0) (Price 0)) `shouldBe` Ask
        
      isBid (Order Bid BTC (Time 0) (Amount 0) (Price 0)) `shouldBe` True
      isAsk (Order Bid BTC (Time 0) (Amount 0) (Price 0)) `shouldBe` False
      isBid (Order Ask BTC (Time 0) (Amount 0) (Price 0)) `shouldBe` False
      isAsk (Order Ask BTC (Time 0) (Amount 0) (Price 0)) `shouldBe` True

    context "when bid is lower than ask" $ do

      it "should not match" $ do

        matchOrders 
          (Order Bid BTC (Time 0) (Amount 1) (Price 10))
          (Order Ask BTC (Time 0) (Amount 1) (Price 20))
            `shouldBe` Nothing

        matchOrders 
          (Order Ask BTC (Time 0) (Amount 1) (Price 20))
          (Order Bid BTC (Time 0) (Amount 1) (Price 10))
            `shouldBe` Nothing

    context "when bid is equal to ask" $ do

      it "should match" $ do

        matchOrders 
          (Order Bid BTC (Time 0) (Amount 2) (Price 15))
          (Order Ask BTC (Time 0) (Amount 1) (Price 15))
            `shouldBe` 
          Just (Trade BTC (Time 0) (Amount 1) (Price 15))

        matchOrders 
          (Order Ask BTC (Time 0) (Amount 2) (Price 15))
          (Order Bid BTC (Time 0) (Amount 1) (Price 15))
            `shouldBe` 
          Just (Trade BTC (Time 0) (Amount 1) (Price 15))

    context "when bid is higher than ask" $ do

      it "should match" $ do

        matchOrders 
          (Order Bid BTC (Time 0) (Amount 2) (Price 20))
          (Order Ask BTC (Time 0) (Amount 1) (Price 10))
            `shouldBe` 
          Just (Trade BTC (Time 0) (Amount 1) (Price 20))

        matchOrders 
          (Order Ask BTC (Time 0) (Amount 2) (Price 10))
          (Order Bid BTC (Time 0) (Amount 1) (Price 20))
            `shouldBe` 
          Just (Trade BTC (Time 0) (Amount 1) (Price 10))
