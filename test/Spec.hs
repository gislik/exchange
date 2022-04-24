import qualified Exchange.Asset as Asset
import qualified Exchange.Order as Order
import qualified Exchange.Book  as Book
import qualified Exchange
import Control.Exception (evaluate)
import Test.QuickCheck (property, forAll, chooseAny, suchThat)
import Exchange.Trade (Trade(Trade))
import Test.Hspec
import Exchange.Type
import Exchange.Entry

main :: IO ()
main = hspec $ do

  describe "Amount" $ do

    it "should be printable" $ do

      show (toAmount 3) `shouldBe` "Amount 3.0"
      show (toAmount 3.0) `shouldBe` "Amount 3.0"
      show (toAmount 2.9) `shouldBe` "Amount 2.9"

    -- it "should be instances of num" $ property $ do
    it "should be instances of num" $ forAll (chooseAny `suchThat` \(x, y) -> x >= 0 && y >= 0 && x >= y) $ do

      -- \x y -> do
      \(x, y) -> do

        (toAmount x) + (toAmount y) `shouldBe` (toAmount $ x+y)
        (toAmount x) - (toAmount y) `shouldBe` (toAmount $ x-y)

    it "should be instances of eq" $ forAll (chooseAny `suchThat` \(x, y) -> x /= y) $ do

      \(x, y) -> do

        (toAmount x) == (toAmount x) `shouldBe` True
        (toAmount x) == (toAmount 2) `shouldBe` False


    it "should be non-negative" $ do

      x <- evaluate (toAmount 0)

      x `shouldBe` toAmount 0

      x <- evaluate (toAmount 1)

      x `shouldBe` toAmount 1

      evaluate (toAmount (-1)) `shouldThrow` anyException


  describe "Trade" $ do

    let
      trade = 
        Trade Asset.BTC (Time 1) (toAmount 2) (Price 3)

    it "should have a nice representation" $ do

      show trade `shouldBe` "Trade BTC (Time 1) (Amount 2.0) (Price 3.0)"

  describe "Order" $ do

    let 
      makers =
        [
          Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 0) (Price 20))
        , Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 0) (Price 10))
        , Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 0) (Price 22))
        , Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 0) (Price 12))
        ]

    it "should have split makers, i.e. bids and asks, to the correct sides" $ do

      Order.splitSides makers `shouldBe` 
        (
          [
            Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 0) (Price 10))
          , Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 0) (Price 12))
          ]
        ,
          [
            Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 0) (Price 20))
          , Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 0) (Price 22))
          ]
        )

    context "when canceling an order" $ do

      it "should be removed if matched" $ do

        let
          maker =
            Order.Maker (Order.limit Ask Asset.BTC (Time 5) (toAmount 0) (Price 22))

        Order.cancel maker makers `shouldBe`
          [
            Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 0) (Price 20))
          , Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 0) (Price 10))
          , Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 0) (Price 12))
          ]


      it "should not be removed if not matched" $ do
        let
          maker =
            Order.Maker (Order.limit Ask Asset.BTC (Time 5) (toAmount 100) (Price 22))

        Order.cancel maker makers `shouldBe`
          [
            Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 0) (Price 20))
          , Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 0) (Price 10))
          , Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 0) (Price 22))
          , Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 0) (Price 12))
          ]

  describe "Order {Limit}" $ do

    let
      btcbid =
        Order.limit Bid Asset.BTC (Time 1) (toAmount 0) (Price 0)
      ethbid =
        Order.limit Bid Asset.ETH (Time 2) (toAmount 0) (Price 0)
      btcask =
        Order.limit Ask Asset.BTC (Time 3) (toAmount 0) (Price 0)
      ethask =
        Order.limit Ask Asset.ETH (Time 4) (toAmount 0) (Price 0)

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


    it "should have a nice representation" $ do

      show btcbid `shouldBe` "Order Bid BTC (Time 1) (Amount 0.0) (Price 0.0)"


    context "when bid is lower than ask {Limit}" $ do

      let 
        maker1 = 
          Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 1) (Price 10))
        taker1 = 
          Order.Taker (Order.limit Ask Asset.BTC (Time 0) (toAmount 1) (Price 20))
        maker2 =
          Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 1) (Price 20))
        taker2 =
          Order.Taker (Order.limit Bid Asset.BTC (Time 0) (toAmount 1) (Price 10))

      it "should not match" $ do

        Order.match maker1 taker1 `shouldBe` Nothing
        Order.match maker2 taker2 `shouldBe` Nothing

      it "should result in no trades and place taker among the makers" $ do

        let
          (makers1, trades1) = 
            Order.trade taker1 [maker1]

        trades1 `shouldBe` []
        makers1 `shouldBe` [maker1, Order.toMaker taker1]

        let
          (makers2, trades2) = 
            Order.trade taker2 [maker2] 

        trades2 `shouldBe` []
        makers2 `shouldBe` [maker2, Order.toMaker taker2]


    context "when bid is equal to ask {Limit}" $ do

      let
        maker1 =
          Order.Maker (Order.limit Bid Asset.BTC (Time 1) (toAmount 2) (Price 15))
        taker1 =
          Order.Taker (Order.limit Ask Asset.BTC (Time 2) (toAmount 1) (Price 15))
        trade1 = 
          Trade Asset.BTC (Time 2) (toAmount 1) (Price 15)
        maker2 =
          Order.Maker (Order.limit Ask Asset.BTC (Time 3) (toAmount 2) (Price 15))
        taker2 =
          Order.Taker (Order.limit Bid Asset.BTC (Time 4) (toAmount 1) (Price 15))
        trade2 = 
          Trade Asset.BTC (Time 4) (toAmount 1) (Price 15)
        maker3 =
          Order.Maker (Order.limit Bid Asset.BTC (Time 5) (toAmount 4) (Price 15))
        taker3 =
          Order.Taker (Order.limit Ask Asset.BTC (Time 6) (toAmount 5) (Price 15))
        trade3 =
            Trade Asset.BTC (Time 6) (toAmount 4) (Price 15)
        maker4 = 
          [maker1, maker3]
        taker4 = 
          taker3
        trades = 
          [
            Trade Asset.BTC (Time 6) (toAmount 2) (Price 15)
          , Trade Asset.BTC (Time 6) (toAmount 3) (Price 15)
          ]
        maker5 =
          Order.Maker (Order.limit Bid Asset.BTC (Time 1) (toAmount 2) (Price 15))
        taker5 =
          Order.Taker (Order.limit Ask Asset.BTC (Time 2) (toAmount 3) (Price 15))
        trade5 = 
          Trade Asset.BTC (Time 2) (toAmount 2) (Price 15)

      it "should match" $ do

        Order.match maker1 taker1 `shouldBe` Just trade1
        Order.match maker2 taker2 `shouldBe` Just trade2

      it "should result in trades and place remaing taker among the makers" $ do

        let
          (makers1, trades1) =
            Order.trade taker1 [maker1] 

        trades1 `shouldBe` [trade1]
        makers1 `shouldBe` [decAmountOf maker1 (amountOf trade1)]

        let
          (makers2, trades2) =
            Order.trade taker2 [maker2] 

        trades2 `shouldBe` [trade2]
        makers2 `shouldBe` [decAmountOf maker2 (amountOf trade2)]

        let
          (makers3, trades3) =
            Order.trade taker3 [maker3] 

        trades3 `shouldBe` [trade3]
        makers3 `shouldBe` [Order.toMaker $ decAmountOf taker3 (amountOf trade3)]

        let
          (makers4, trades4) =
            Order.trade taker4 maker4 

        trades4 `shouldBe` trades 
        makers4 `shouldBe` [setAmountOf maker3 1]

        let
          (makers5, trades5) =
            Order.trade taker5 [maker5] 

        trades5 `shouldBe` [trade5]
        makers5 `shouldBe` [Order.toMaker $ decAmountOf taker5 (amountOf trade5)]

    context "when bid is higher than ask {Limit}" $ do

      let
        maker1 =
          Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 20))
        taker1 =
          Order.Taker (Order.limit Ask Asset.BTC (Time 0) (toAmount 1) (Price 10))
        trade1 =
          Trade Asset.BTC (Time 0) (toAmount 1) (Price 20)
        maker2 =
          Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 2) (Price 10))
        taker2 =
          Order.Taker (Order.limit Bid Asset.BTC (Time 0) (toAmount 1) (Price 20))
        trade2 =
          Trade Asset.BTC (Time 0) (toAmount 1) (Price 10)
        maker3 =
          Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 8) (Price 15))
        taker3 =
          Order.Taker (Order.limit Bid Asset.BTC (Time 0) (toAmount 10) (Price 20))
        trades =
          [
            Trade Asset.BTC (Time 0) (toAmount 2) (Price 10)
          , Trade Asset.BTC (Time 0) (toAmount 8) (Price 15)
          ]

      it "should match" $ do

        Order.match maker1 taker1 `shouldBe` Just trade1
        Order.match maker2 taker2 `shouldBe` Just trade2

      it "should result in trades" $ do

        let
          (makers1, trades1) =
            Order.trade taker1 [maker1] 

        trades1 `shouldBe` [trade1]
        makers1 `shouldBe` [decAmountOf maker1 (amountOf trade1)]

        let
          (makers2, trades2) =
            Order.trade taker2 [maker2] 

        trades2 `shouldBe` [trade2]
        makers2 `shouldBe` [decAmountOf maker2 (amountOf trade2)]

        -- let
          -- (makers3, trades3) =
            -- Order.trade taker3 [maker3] 

        -- trades3 `shouldBe` [trade3]
        -- makers3 `shouldBe` []

        let
          (makers4, trades4) =
            Order.trade taker3 [maker2, maker3] 

        trades4 `shouldBe` trades
        makers4 `shouldBe` []


  describe "Order {AllOrNothing}" $ do

    context "when bid is lower than ask {AllOrNothing}" $ do

      let 
        maker1 = 
          Order.Maker (Order.allOrNothing Bid Asset.BTC (Time 0) (toAmount 1) (Price 10))
        taker1 = 
          Order.Taker (Order.allOrNothing Ask Asset.BTC (Time 0) (toAmount 1) (Price 20))
        maker2 =
          Order.Maker (Order.allOrNothing Ask Asset.BTC (Time 0) (toAmount 1) (Price 20))
        taker2 =
          Order.Taker (Order.allOrNothing Bid Asset.BTC (Time 0) (toAmount 1) (Price 10))

      it "should result in no trades and unchanged makers" $ do

        let
          (makers1, trades1) = 
            Order.trade taker1 [maker1] 

        trades1 `shouldBe` []
        makers1 `shouldBe` [maker1]

        let
          (makers2, trades2) = 
            Order.trade taker2 [maker2] 

        trades2 `shouldBe` []
        makers2 `shouldBe` [maker2]

    context "when bid is equal to ask {AllOrNothing}" $ do

      let
        maker1 =
          Order.Maker (Order.allOrNothing Bid Asset.BTC (Time 1) (toAmount 2) (Price 15))
        taker1 =
          Order.Taker (Order.allOrNothing Ask Asset.BTC (Time 2) (toAmount 1) (Price 15))
        trade1 = 
          Trade Asset.BTC (Time 2) (toAmount 1) (Price 15)
        maker2 =
          Order.Maker (Order.allOrNothing Ask Asset.BTC (Time 3) (toAmount 2) (Price 15))
        taker2 =
          Order.Taker (Order.allOrNothing Bid Asset.BTC (Time 4) (toAmount 1) (Price 15))
        trade2 = 
          Trade Asset.BTC (Time 4) (toAmount 1) (Price 15)
        maker3 =
          Order.Maker (Order.allOrNothing Bid Asset.BTC (Time 5) (toAmount 4) (Price 15))
        taker3 =
          Order.Taker (Order.allOrNothing Ask Asset.BTC (Time 6) (toAmount 5) (Price 15))
        trade3 =
            Trade Asset.BTC (Time 6) (toAmount 4) (Price 15)
        trades = 
          [
            Trade Asset.BTC (Time 6) (toAmount 2) (Price 15)
          , Trade Asset.BTC (Time 6) (toAmount 3) (Price 15)
          ]

      it "should result in trades" $ do

        let
          (makers1, trades1) =
            Order.trade taker1 [maker1] 

        trades1 `shouldBe` [trade1]
        makers1 `shouldBe` [decAmountOf maker1 (amountOf trade1)]

        let
          (makers2, trades2) =
            Order.trade taker2 [maker2] 

        trades2 `shouldBe` [trade2]
        makers2 `shouldBe` [decAmountOf maker2 (amountOf trade2)]

        let
          (makers3, trades3) =
            Order.trade taker3 [maker3] 

        trades3 `shouldBe` [trade3]
        makers3 `shouldBe` []

        let
          (makers4, trades4) =
            Order.trade taker3 [maker1, maker3]

        trades4 `shouldBe` trades 
        makers4 `shouldBe` [setAmountOf maker3 1]

  describe "Book" $ do

    context "when an order is placed" $ do

      let
        book = 
          foldr Book.newOrder Book.empty 
            [
              Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 10))
            , Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 2) (Price 20))
            ]
        bid =
          Order.Taker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 15))
        ask =
          Order.Taker (Order.limit Ask Asset.BTC (Time 0) (toAmount 2) (Price 15))

      it "should end up on the correct side" $ do

        let
          (book', trades) =
            Book.trade bid book
          bids = 
            Book.bids book'
          asks = 
            Book.asks book'

        bids `shouldBe` 
          [
            Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 15))
          , Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 10))
          ]

  describe "Exchange" $ do

    context "when a deposit is made" $ do

      it "should update the balance" $ do

        balance <- Exchange.run $ do
          Exchange.balance

        balance `shouldBe` 0

        balance <- Exchange.run $ do
          Exchange.deposit 100
          Exchange.balance

        balance `shouldBe` 100

    context "when a withdrawal is made" $ do

      it "should update the balance" $ do

        balance <- Exchange.run $ do
          Exchange.deposit 100
          Exchange.withdraw 40
          Exchange.balance

        balance `shouldBe` 60

      it "should result in an error if balance becomes negative" $ do

        Exchange.run (Exchange.withdraw 100) `shouldThrow` anyException

    context "when no orders have been placed" $ do

      it "should have an empty book" $ do

        book' <- Exchange.run Exchange.orderbook
        length book' `shouldBe` 0

    context "when a single order has been placed" $ do

      let 
        book = 
          foldr Book.newOrder Book.empty 
            [
              Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 10))
            ]

      it "should have a book with one entry" $ do

        book' <- Exchange.runWith book Exchange.orderbook
        length book' `shouldBe` 1


    context "when two orders has been placed" $ do

      let
        book = 
          foldr Book.newOrder Book.empty 
            [
              Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 10))
            , Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 2) (Price 20))
            ]

      it "should have a book with two entries" $ do

        book' <- Exchange.runWith book Exchange.orderbook
        length book' `shouldBe` 2

    context "when the balance is less than trade cost" $ do

      let
        book = 
          foldr Book.newOrder Book.empty
            [
              Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 10))
            , Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 2) (Price 20))
            ]

      it "should result in an error" $ do

        let 
          bid =
            Order.Taker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 20))

        Exchange.runWith book (Exchange.trade bid) `shouldThrow` anyException



    context "when a trade happens" $ do

      let
        book = 
          foldr Book.newOrder Book.empty 
            [
              Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 10))
            , Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 2) (Price 20))
            ]

      it "should have a book with a single entry left" $ do

        book' <- Exchange.runWith book $ do
          Exchange.deposit 100
          Exchange.trade (Order.Taker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 30)))
          Exchange.orderbook
        length book' `shouldBe` 1

        book' <- Exchange.runWith book $ do
          Exchange.deposit 100
          Exchange.trade (Order.Taker (Order.limit Ask Asset.BTC (Time 0) (toAmount 2) (Price 5)))
          Exchange.orderbook
        length book' `shouldBe` 1

      it "should have a trade in the exchange state" $ do

        trades <- Exchange.runWith book $ do
          Exchange.deposit 100
          Exchange.trade (Order.Taker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 30)))
        length trades `shouldBe` 1

      it "should be appended to the blotter" $ do

        trades <- Exchange.runWith book $ do
          Exchange.deposit 100
          Exchange.trade (Order.Taker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 30)))
          Exchange.blotter
        length trades `shouldBe ` 1

        trades <- Exchange.runWith book $ do
          Exchange.deposit 100
          Exchange.trade (Order.Taker (Order.limit Bid Asset.BTC (Time 0) (toAmount 1) (Price 30)))
          Exchange.trade (Order.Taker (Order.limit Bid Asset.BTC (Time 0) (toAmount 1) (Price 30)))
          Exchange.blotter
        length trades `shouldBe ` 2


    context "when a limit order isn't fully matched" $ do

      let
        book = 
          foldr Book.newOrder Book.empty 
            [
              Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 10))
            , Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 2) (Price 20))
            ]

      it "should place the rest of the order on the correct side" $ do

        let 
          bid =
            Order.Taker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 15))

        book' <- Exchange.runWith book $ do
          Exchange.deposit 100
          Exchange.trade bid
          Exchange.orderbook
        book' `shouldBe` foldr Book.newOrder book [Order.toMaker bid]

        let 
          ask =
            Order.Taker (Order.limit Ask Asset.BTC (Time 0) (toAmount 2) (Price 15))

        book' <- Exchange.runWith book $ do
          Exchange.deposit 100
          Exchange.trade ask
          Exchange.orderbook
        book' `shouldBe` foldr Book.newOrder book [Order.toMaker ask]

    context "when an order is canceled" $ do

      let
        book = 
          foldr Book.newOrder Book.empty 
            [
              Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 10))
            , Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 2) (Price 20))
            ]

      it "should be removed" $ do
        let
          maker =
            Order.Maker (Order.limit Ask Asset.BTC (Time 0) (toAmount 2) (Price 20))

        book' <- Exchange.runWith book $ do
          Exchange.cancel maker
          Exchange.orderbook

        Book.bids book' `shouldBe` 
          [
            Order.Maker (Order.limit Bid Asset.BTC (Time 0) (toAmount 2) (Price 10))
          ]

