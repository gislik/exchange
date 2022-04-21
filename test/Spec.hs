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

      let 
        maker1 = 
          Maker (Order Bid BTC (Time 0) (Amount 1) (Price 10))
        taker1 = 
          Taker (Order Ask BTC (Time 0) (Amount 1) (Price 20))
        maker2 =
          Maker (Order Ask BTC (Time 0) (Amount 1) (Price 20))
        taker2 =
          Taker (Order Bid BTC (Time 0) (Amount 1) (Price 10))

      it "should not match" $ do
      
        matchOrders maker1 taker1 `shouldBe` Nothing
        matchOrders maker2 taker2 `shouldBe` Nothing

      it "should result in no trades and unchanged makers" $ do

        let
          (makers1, trades1) = 
            tradeOrders [maker1] taker1

        trades1 `shouldBe` []
        makers1 `shouldBe` [maker1]

        let
          (makers2, trades2) = 
            tradeOrders [maker2] taker2

        trades2 `shouldBe` []
        makers2 `shouldBe` [maker2]


    context "when bid is equal to ask" $ do

      let
        maker1 =
          Maker (Order Bid BTC (Time 0) (Amount 2) (Price 15))
        taker1 =
          Taker (Order Ask BTC (Time 0) (Amount 1) (Price 15))
        maker2 =
          Maker (Order Ask BTC (Time 0) (Amount 2) (Price 15))
        taker2 =
          Taker (Order Bid BTC (Time 0) (Amount 1) (Price 15))
        trade = 
          Trade BTC (Time 0) (Amount 1) (Price 15)
        maker3 =
          Maker (Order Bid BTC (Time 0) (Amount 4) (Price 15))
        taker3 =
          Taker (Order Ask BTC (Time 0) (Amount 5) (Price 15))
        trade3 =
            Trade BTC (Time 0) (Amount 4) (Price 15)
        trades = 
          [
            Trade BTC (Time 0) (Amount 2) (Price 15)
          , Trade BTC (Time 0) (Amount 3) (Price 15)
          ]

      it "should match" $ do

        matchOrders maker1 taker1 `shouldBe` Just trade
        matchOrders maker2 taker2 `shouldBe` Just trade

      it "should result in trades" $ do

        let
          (makers1, trades1) =
            tradeOrders [maker1] taker1

        trades1 `shouldBe` [trade]
        makers1 `shouldBe` [decAmountOf maker1 (amountOf trade)]

        let
          (makers2, trades2) =
            tradeOrders [maker2] taker2

        trades2 `shouldBe` [trade]
        makers2 `shouldBe` [decAmountOf maker2 (amountOf trade)]

        let
          (makers3, trades3) =
            tradeOrders [maker3] taker3

        trades3 `shouldBe` [trade3]
        makers3 `shouldBe` []

        let
          (makers4, trades4) =
            tradeOrders [maker1, maker3] taker3

        trades4 `shouldBe` trades
        makers4 `shouldBe` [setAmountOf maker3 1]

    context "when bid is higher than ask" $ do

      let
        maker1 =
          Maker (Order Bid BTC (Time 0) (Amount 2) (Price 20))
        taker1 =
          Taker (Order Ask BTC (Time 0) (Amount 1) (Price 10))
        trade1 =
          Trade BTC (Time 0) (Amount 1) (Price 20)
        maker2 =
          Maker (Order Ask BTC (Time 0) (Amount 2) (Price 10))
        taker2 =
          Taker (Order Bid BTC (Time 0) (Amount 1) (Price 20))
        trade2 =
          Trade BTC (Time 0) (Amount 1) (Price 10)
        maker3 =
          Maker (Order Ask BTC (Time 0) (Amount 8) (Price 15))
        taker3 =
          Taker (Order Bid BTC (Time 0) (Amount 10) (Price 20))
        trades =
          [
            Trade BTC (Time 0) (Amount 2) (Price 10)
          , Trade BTC (Time 0) (Amount 8) (Price 15)
          ]

      it "should match" $ do

        matchOrders maker1 taker1 `shouldBe` Just trade1
        matchOrders maker2 taker2 `shouldBe` Just trade2

      it "should result in trades" $ do
        
        let
          (makers1, trades1) =
            tradeOrders [maker1] taker1

        trades1 `shouldBe` [trade1]
        makers1 `shouldBe` [decAmountOf maker1 (amountOf trade1)]

        let
          (makers2, trades2) =
            tradeOrders [maker2] taker2

        trades2 `shouldBe` [trade2]
        makers2 `shouldBe` [decAmountOf maker2 (amountOf trade2)]

        -- let
          -- (makers3, trades3) =
            -- tradeOrders [maker3] taker3

        -- trades3 `shouldBe` [trade3]
        -- makers3 `shouldBe` []

        let
          (makers4, trades4) =
            tradeOrders [maker2, maker3] taker3

        trades4 `shouldBe` trades
        makers4 `shouldBe` []

  describe "Exchange" $ do
    
    context "when no orders have been placed" $ do

      it "should have an empty book" $ do

        length (runExchange exchangeBook) `shouldBe` 0
        
    context "when a single order has been placed" $ do

      let 
        book = 
          foldr newOrder emptyBook 
            [
              Order Bid BTC (Time 0) (Amount 2) (Price 10)
            ]

      it "should have a book with one entry" $ do

        length (runExchangeWith book exchangeBook) `shouldBe` 1


    context "when two orders has been placed" $ do

      let
        book = 
          foldr newOrder emptyBook 
            [
              Order Bid BTC (Time 0) (Amount 2) (Price 10)
            , Order Ask BTC (Time 0) (Amount 2) (Price 20)
            ]

      it "should have a book with two entries" $ do

        length (runExchangeWith book exchangeBook) `shouldBe` 2
