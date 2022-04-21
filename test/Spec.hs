import qualified Exchange.Asset as Asset
import qualified Exchange.Order as Order
import qualified Exchange.Book  as Book
import Test.QuickCheck 
import Test.Hspec
import Exchange.Trade (Trade(Trade))
import Exchange.Order (Order(Order))
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

    let
      btcbid =
        Order Bid Asset.BTC (Time 1) (Amount 0) (Price 0)
      ethbid =
        Order Bid Asset.ETH (Time 2) (Amount 0) (Price 0)
      btcask =
        Order Ask Asset.BTC (Time 3) (Amount 0) (Price 0)
      ethask =
        Order Ask Asset.ETH (Time 4) (Amount 0) (Price 0)

    it "should have an asset" $ do

      assetOf btcbid `shouldBe` Asset.BTC
      assetOf ethbid `shouldBe` Asset.ETH
      assetOf btcask `shouldBe` Asset.BTC
      assetOf ethask `shouldBe` Asset.ETH

    it "should have correct sides" $ do

      sideOf btcbid `shouldBe` Bid
      sideOf btcask `shouldBe` Ask
        
      Order.isBid btcbid `shouldBe` True
      Order.isAsk btcbid `shouldBe` False
      Order.isBid btcask `shouldBe` False
      Order.isAsk btcask `shouldBe` True

    it "should have time" $ do

      timeOf btcbid `shouldBe` (Time 1)
      timeOf ethbid `shouldBe` (Time 2)
      timeOf btcask `shouldBe` (Time 3)
      timeOf ethask `shouldBe` (Time 4)

    context "when bid is lower than ask" $ do

      let 
        maker1 = 
          Order.Maker (Order Bid Asset.BTC (Time 0) (Amount 1) (Price 10))
        taker1 = 
          Order.Taker (Order Ask Asset.BTC (Time 0) (Amount 1) (Price 20))
        maker2 =
          Order.Maker (Order Ask Asset.BTC (Time 0) (Amount 1) (Price 20))
        taker2 =
          Order.Taker (Order Bid Asset.BTC (Time 0) (Amount 1) (Price 10))

      it "should not match" $ do
      
        Order.match maker1 taker1 `shouldBe` Nothing
        Order.match maker2 taker2 `shouldBe` Nothing

      it "should result in no trades and unchanged makers" $ do

        let
          (makers1, trades1) = 
            Order.trade [maker1] taker1

        trades1 `shouldBe` []
        makers1 `shouldBe` [maker1]

        let
          (makers2, trades2) = 
            Order.trade [maker2] taker2

        trades2 `shouldBe` []
        makers2 `shouldBe` [maker2]


    context "when bid is equal to ask" $ do

      let
        maker1 =
          Order.Maker (Order Bid Asset.BTC (Time 1) (Amount 2) (Price 15))
        taker1 =
          Order.Taker (Order Ask Asset.BTC (Time 2) (Amount 1) (Price 15))
        trade1 = 
          Trade Asset.BTC (Time 2) (Amount 1) (Price 15)
        maker2 =
          Order.Maker (Order Ask Asset.BTC (Time 3) (Amount 2) (Price 15))
        taker2 =
          Order.Taker (Order Bid Asset.BTC (Time 4) (Amount 1) (Price 15))
        trade2 = 
          Trade Asset.BTC (Time 4) (Amount 1) (Price 15)
        maker3 =
          Order.Maker (Order Bid Asset.BTC (Time 5) (Amount 4) (Price 15))
        taker3 =
          Order.Taker (Order Ask Asset.BTC (Time 6) (Amount 5) (Price 15))
        trade3 =
            Trade Asset.BTC (Time 6) (Amount 4) (Price 15)
        trades = 
          [
            Trade Asset.BTC (Time 6) (Amount 2) (Price 15)
          , Trade Asset.BTC (Time 6) (Amount 3) (Price 15)
          ]

      it "should match" $ do

        Order.match maker1 taker1 `shouldBe` Just trade1
        Order.match maker2 taker2 `shouldBe` Just trade2

      it "should result in trades" $ do

        let
          (makers1, trades1) =
            Order.trade [maker1] taker1

        trades1 `shouldBe` [trade1]
        makers1 `shouldBe` [decAmountOf maker1 (amountOf trade1)]

        let
          (makers2, trades2) =
            Order.trade [maker2] taker2

        trades2 `shouldBe` [trade2]
        makers2 `shouldBe` [decAmountOf maker2 (amountOf trade2)]

        let
          (makers3, trades3) =
            Order.trade [maker3] taker3

        trades3 `shouldBe` [trade3]
        makers3 `shouldBe` []

        let
          (makers4, trades4) =
            Order.trade [maker1, maker3] taker3

        trades4 `shouldBe` trades 
        makers4 `shouldBe` [setAmountOf maker3 1]

    context "when bid is higher than ask" $ do

      let
        maker1 =
          Order.Maker (Order Bid Asset.BTC (Time 0) (Amount 2) (Price 20))
        taker1 =
          Order.Taker (Order Ask Asset.BTC (Time 0) (Amount 1) (Price 10))
        trade1 =
          Trade Asset.BTC (Time 0) (Amount 1) (Price 20)
        maker2 =
          Order.Maker (Order Ask Asset.BTC (Time 0) (Amount 2) (Price 10))
        taker2 =
          Order.Taker (Order Bid Asset.BTC (Time 0) (Amount 1) (Price 20))
        trade2 =
          Trade Asset.BTC (Time 0) (Amount 1) (Price 10)
        maker3 =
          Order.Maker (Order Ask Asset.BTC (Time 0) (Amount 8) (Price 15))
        taker3 =
          Order.Taker (Order Bid Asset.BTC (Time 0) (Amount 10) (Price 20))
        trades =
          [
            Trade Asset.BTC (Time 0) (Amount 2) (Price 10)
          , Trade Asset.BTC (Time 0) (Amount 8) (Price 15)
          ]

      it "should match" $ do

        Order.match maker1 taker1 `shouldBe` Just trade1
        Order.match maker2 taker2 `shouldBe` Just trade2

      it "should result in trades" $ do
        
        let
          (makers1, trades1) =
            Order.trade [maker1] taker1

        trades1 `shouldBe` [trade1]
        makers1 `shouldBe` [decAmountOf maker1 (amountOf trade1)]

        let
          (makers2, trades2) =
            Order.trade [maker2] taker2

        trades2 `shouldBe` [trade2]
        makers2 `shouldBe` [decAmountOf maker2 (amountOf trade2)]

        -- let
          -- (makers3, trades3) =
            -- Order.trade [maker3] taker3

        -- trades3 `shouldBe` [trade3]
        -- makers3 `shouldBe` []

        let
          (makers4, trades4) =
            Order.trade [maker2, maker3] taker3

        trades4 `shouldBe` trades
        makers4 `shouldBe` []

  describe "Exchange" $ do
    
    context "when no orders have been placed" $ do

      it "should have an empty book" $ do

        book' <- run orderbook
        length book' `shouldBe` 0
        
    context "when a single order has been placed" $ do

      let 
        book = 
          foldr Book.newOrder Book.empty 
            [
              Order.Maker (Order Bid Asset.BTC (Time 0) (Amount 2) (Price 10))
            ]

      it "should have a book with one entry" $ do

        book' <- runWith book orderbook
        length book' `shouldBe` 1


    context "when two orders has been placed" $ do

      let
        book = 
          foldr Book.newOrder Book.empty 
            [
              Order.Maker (Order Bid Asset.BTC (Time 0) (Amount 2) (Price 10))
            , Order.Maker (Order Ask Asset.BTC (Time 0) (Amount 2) (Price 20))
            ]

      it "should have a book with two entries" $ do

        book' <- runWith book orderbook
        length book' `shouldBe` 2
