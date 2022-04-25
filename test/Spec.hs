import qualified Exchange.Trade as Trade
import qualified Exchange.Order as Order
import qualified Exchange.Book  as Book
import qualified Exchange
import Control.Exception (evaluate)
import Test.QuickCheck (property, forAll, chooseAny, suchThat)
import Exchange.Trade (Trade(Trade))
import Test.Hspec
import Exchange.Type
import Exchange.Entry
import Exchange.Asset

main :: IO ()
main = hspec $ do

  describe "Amount" $ do

    it "should be printable" $ do

      show ((toAmount 3) :: Amount BTC) `shouldBe` "3.0 BTC"
      show ((toAmount 3.0) :: Amount BTC)`shouldBe` "3.0 BTC"
      show ((toAmount 2.9) :: Amount BTC) `shouldBe` "2.9 BTC"

    -- it "should be instances of num" $ property $ do
    it "should be instances of num" $ forAll (chooseAny `suchThat` \(x, y) -> x >= 0 && y >= 0 && x >= y) $ do

      -- \x y -> do
      \(x, y) -> do

        (toAmount x) + (toAmount y) `shouldBe` ((toAmount $ x+y) :: Amount BTC)
        (toAmount x) - (toAmount y) `shouldBe` ((toAmount $ x-y) :: Amount BTC)

    it "should be instances of eq" $ forAll (chooseAny `suchThat` \(x, y) -> x /= y) $ do

      \(x, y) -> do

        (toAmount x) == (toAmount x) `shouldBe` True
        (toAmount x) == (toAmount 2) `shouldBe` False


    it "should be non-negative" $ do

      x <- evaluate (toAmount 0)

      (x :: Amount BTC) `shouldBe` toAmount 0

      x <- evaluate (toAmount 1)

      (x :: Amount BTC) `shouldBe` toAmount 1

      evaluate (toAmount (-1)) `shouldThrow` anyException


  describe "Price" $ do

    it "should be non-negative" $ do

      x <- evaluate (toPrice 0)

      (x :: Price BTC) `shouldBe` toPrice 0

      x <- evaluate (toPrice 1)

      (x :: Price BTC) `shouldBe` toPrice 1

      evaluate (toPrice (-1)) `shouldThrow` anyException


  describe "Trade" $ do

    let
      trade =
        Trade.new (Time 1) (toAmount 2) BTC (toPrice 3) USD

    it "should have a nice representation" $ do

      show trade `shouldBe` "Trade (Time 1) 2.0 BTC @ 3.0 USD"

  describe "Order" $ do

    let
      makers =
        [
          Order.Maker (Order.limit Ask (Time 0) (toAmount 0) BTC (toPrice 20) USD)
        , Order.Maker (Order.limit Bid (Time 0) (toAmount 0) BTC (toPrice 10) USD)
        , Order.Maker (Order.limit Ask (Time 0) (toAmount 0) BTC (toPrice 22) USD)
        , Order.Maker (Order.limit Bid (Time 0) (toAmount 0) BTC (toPrice 12) USD)
        ]

    it "should have split makers, i.e. bids and asks, to the correct sides" $ do

      Order.splitSides makers `shouldBe`
        (
          [
            Order.Maker (Order.limit Bid (Time 0) (toAmount 0) BTC (toPrice 10) USD)
          , Order.Maker (Order.limit Bid (Time 0) (toAmount 0) BTC (toPrice 12) USD)
          ]
        ,
          [
            Order.Maker (Order.limit Ask (Time 0) (toAmount 0) BTC (toPrice 20) USD)
          , Order.Maker (Order.limit Ask (Time 0) (toAmount 0) BTC (toPrice 22) USD)
          ]
        )

    context "when canceling an order" $ do

      it "should be removed if matched" $ do

        let
          maker =
            Order.Maker (Order.limit Ask (Time 5) (toAmount 0) BTC (toPrice 22) USD)

        Order.cancel maker makers `shouldBe`
          [
            Order.Maker (Order.limit Ask (Time 0) (toAmount 0) BTC (toPrice 20) USD)
          , Order.Maker (Order.limit Bid (Time 0) (toAmount 0) BTC (toPrice 10) USD)
          , Order.Maker (Order.limit Bid (Time 0) (toAmount 0) BTC (toPrice 12) USD)
          ]


      it "should not be removed if not matched" $ do
        let
          maker =
            Order.Maker (Order.limit Ask (Time 5) (toAmount 100) BTC (toPrice 22) USD)

        Order.cancel maker makers `shouldBe`
          [
            Order.Maker (Order.limit Ask (Time 0) (toAmount 0) BTC (toPrice 20) USD)
          , Order.Maker (Order.limit Bid (Time 0) (toAmount 0) BTC (toPrice 10) USD)
          , Order.Maker (Order.limit Ask (Time 0) (toAmount 0) BTC (toPrice 22) USD)
          , Order.Maker (Order.limit Bid (Time 0) (toAmount 0) BTC (toPrice 12) USD)
          ]

  describe "Order {Limit}" $ do

    let
      btcbid =
        Order.limit Bid (Time 1) (toAmount 0) BTC (toPrice 0) USD
      ethbid =
        Order.limit Bid (Time 2) (toAmount 0) ETH (toPrice 0) USD
      btcask =
        Order.limit Ask (Time 3) (toAmount 0) BTC (toPrice 0) USD
      ethask =
        Order.limit Ask (Time 4) (toAmount 0) ETH (toPrice 0) USD

    it "should have an asset" $ do

      baseOf btcbid `shouldBe` BTC
      baseOf ethbid `shouldBe` ETH
      baseOf btcask `shouldBe` BTC
      baseOf ethask `shouldBe` ETH

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

      show btcbid `shouldBe` "Order Bid BTC (Time 1) 0.0 BTC 0.0 USD"


    context "when bid is lower than ask {Limit}" $ do

      let
        maker1 =
          Order.Maker (Order.limit Bid (Time 0) (toAmount 1) BTC (toPrice 10) USD)
        taker1 =
          Order.Taker (Order.limit Ask (Time 0) (toAmount 1) BTC (toPrice 20) USD)
        maker2 =
          Order.Maker (Order.limit Ask (Time 0) (toAmount 1) BTC (toPrice 20) USD)
        taker2 =
          Order.Taker (Order.limit Bid (Time 0) (toAmount 1) BTC (toPrice 10) USD)

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
          Order.Maker (Order.limit Bid (Time 1) (toAmount 2) BTC (toPrice 15) USD)
        taker1 =
          Order.Taker (Order.limit Ask (Time 2) (toAmount 1) BTC (toPrice 15) USD)
        trade1 =
          Trade.new (Time 2) (toAmount 1) BTC (toPrice 15) USD
        maker2 =
          Order.Maker (Order.limit Ask (Time 3) (toAmount 2) BTC (toPrice 15) USD)
        taker2 =
          Order.Taker (Order.limit Bid (Time 4) (toAmount 1) BTC (toPrice 15) USD)
        trade2 =
          Trade.new (Time 4) (toAmount 1) BTC (toPrice 15) USD
        maker3 =
          Order.Maker (Order.limit Bid (Time 5) (toAmount 4) BTC (toPrice 15) USD)
        taker3 =
          Order.Taker (Order.limit Ask (Time 6) (toAmount 5) BTC (toPrice 15) USD)
        trade3 =
            Trade.new (Time 6) (toAmount 4) BTC (toPrice 15) USD
        maker4 =
          [maker1, maker3]
        taker4 =
          taker3
        trades =
          [
            Trade.new (Time 6) (toAmount 2) BTC (toPrice 15) USD
          , Trade.new (Time 6) (toAmount 3) BTC (toPrice 15) USD
          ]
        maker5 =
          Order.Maker (Order.limit Bid (Time 1) (toAmount 2) BTC (toPrice 15) USD)
        taker5 =
          Order.Taker (Order.limit Ask (Time 2) (toAmount 3) BTC (toPrice 15) USD)
        trade5 =
          Trade.new (Time 2) (toAmount 2) BTC (toPrice 15) USD

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
          Order.Maker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 20) USD)
        taker1 =
          Order.Taker (Order.limit Ask (Time 0) (toAmount 1) BTC (toPrice 10) USD)
        trade1 =
          Trade.new (Time 0) (toAmount 1) BTC (toPrice 20) USD
        maker2 =
          Order.Maker (Order.limit Ask (Time 0) (toAmount 2) BTC (toPrice 10) USD)
        taker2 =
          Order.Taker (Order.limit Bid (Time 0) (toAmount 1) BTC (toPrice 20) USD)
        trade2 =
          Trade.new (Time 0) (toAmount 1) BTC (toPrice 10) USD
        maker3 =
          Order.Maker (Order.limit Ask (Time 0) (toAmount 8) BTC (toPrice 15) USD)
        taker3 =
          Order.Taker (Order.limit Bid (Time 0) (toAmount 10) BTC (toPrice 20) USD)
        trades =
          [
            Trade.new (Time 0) (toAmount 2) BTC (toPrice 10) USD
          , Trade.new (Time 0) (toAmount 8) BTC (toPrice 15) USD
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
          Order.Maker (Order.allOrNothing Bid (Time 0) (toAmount 1) BTC (toPrice 10) USD)
        taker1 =
          Order.Taker (Order.allOrNothing Ask (Time 0) (toAmount 1) BTC (toPrice 20) USD)
        maker2 =
          Order.Maker (Order.allOrNothing Ask (Time 0) (toAmount 1) BTC (toPrice 20) USD)
        taker2 =
          Order.Taker (Order.allOrNothing Bid (Time 0) (toAmount 1) BTC (toPrice 10) USD)

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
          Order.Maker (Order.allOrNothing Bid (Time 1) (toAmount 2) BTC (toPrice 15) USD)
        taker1 =
          Order.Taker (Order.allOrNothing Ask (Time 2) (toAmount 1) BTC (toPrice 15) USD)
        trade1 =
          Trade.new (Time 2) (toAmount 1) BTC (toPrice 15) USD
        maker2 =
          Order.Maker (Order.allOrNothing Ask (Time 3) (toAmount 2) BTC (toPrice 15) USD)
        taker2 =
          Order.Taker (Order.allOrNothing Bid (Time 4) (toAmount 1) BTC (toPrice 15) USD)
        trade2 =
          Trade.new (Time 4) (toAmount 1) BTC (toPrice 15) USD
        maker3 =
          Order.Maker (Order.allOrNothing Bid (Time 5) (toAmount 4) BTC (toPrice 15) USD)
        taker3 =
          Order.Taker (Order.allOrNothing Ask (Time 6) (toAmount 5) BTC (toPrice 15) USD)
        trade3 =
            Trade.new (Time 6) (toAmount 4) BTC (toPrice 15) USD
        trades =
          [
            Trade.new (Time 6) (toAmount 2) BTC (toPrice 15) USD
          , Trade.new (Time 6) (toAmount 3) BTC (toPrice 15) USD
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
              Order.Maker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 10) USD)
            , Order.Maker (Order.limit Ask (Time 0) (toAmount 2) BTC (toPrice 20) USD)
            ]
        bid =
          Order.Taker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 15) USD)
        ask =
          Order.Taker (Order.limit Ask (Time 0) (toAmount 2) BTC (toPrice 15) USD)

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
            Order.Maker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 15) USD)
          , Order.Maker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 10) USD)
          ]

  describe "Exchange" $ do

    context "when a deposit is made" $ do

      it "should update the balance" $ do

        balance <- Exchange.run $ do
          Exchange.balance 

        (balance :: Amount USD) `shouldBe` 0

        balance <- Exchange.run $ do
          Exchange.deposit (100 :: Amount USD)
          Exchange.balance

        balance `shouldBe` 100

    context "when a withdrawal is made" $ do

      it "should update the balance" $ do

        balance <- Exchange.run $ do
          Exchange.deposit (100 :: Amount USD)
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
              Order.Maker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 10) USD)
            ]

      it "should have a book with one entry" $ do

        book' <- Exchange.runWith book Exchange.orderbook
        length book' `shouldBe` 1


    context "when two orders has been placed" $ do

      let
        book =
          foldr Book.newOrder Book.empty
            [
              Order.Maker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 10) USD)
            , Order.Maker (Order.limit Ask (Time 0) (toAmount 2) BTC (toPrice 20) USD)
            ]

      it "should have a book with two entries" $ do

        book' <- Exchange.runWith book Exchange.orderbook
        length book' `shouldBe` 2

    context "when the balance is less than trade cost" $ do

      let
        book =
          foldr Book.newOrder Book.empty
            [
              Order.Maker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 10) USD)
            , Order.Maker (Order.limit Ask (Time 0) (toAmount 2) BTC (toPrice 20) USD)
            ]

      it "should result in an error" $ do

        let
          bid =
            Order.Taker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 20) USD)

        Exchange.runWith book (Exchange.trade bid) `shouldThrow` anyException



    context "when a trade happens" $ do

      let
        book =
          foldr Book.newOrder Book.empty
            [
              Order.Maker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 10) USD)
            , Order.Maker (Order.limit Ask (Time 0) (toAmount 2) BTC (toPrice 20) USD)
            ]

      it "should have a book with a single entry left" $ do

        book' <- Exchange.runWith book $ do
          Exchange.deposit 100
          Exchange.trade (Order.Taker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 30) USD))
          Exchange.orderbook
        length book' `shouldBe` 1

        book' <- Exchange.runWith book $ do
          Exchange.deposit 100
          Exchange.trade (Order.Taker (Order.limit Ask (Time 0) (toAmount 2) BTC (toPrice 5) USD))
          Exchange.orderbook
        length book' `shouldBe` 1

      it "should have a trade in the exchange state" $ do

        trades <- Exchange.runWith book $ do
          Exchange.deposit 100
          Exchange.trade (Order.Taker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 30) USD))
        length trades `shouldBe` 1

      it "should be appended to the blotter" $ do

        trades <- Exchange.runWith book $ do
          Exchange.deposit 100
          Exchange.trade (Order.Taker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 30) USD))
          Exchange.blotter
        length trades `shouldBe ` 1

        trades <- Exchange.runWith book $ do
          Exchange.deposit 100
          Exchange.trade (Order.Taker (Order.limit Bid (Time 0) (toAmount 1) BTC (toPrice 30) USD))
          Exchange.trade (Order.Taker (Order.limit Bid (Time 0) (toAmount 1) BTC (toPrice 30) USD))
          Exchange.blotter
        length trades `shouldBe ` 2


      it "should update the balance" $ do

        balance <- Exchange.runWith book $ do
          Exchange.deposit 100
          Exchange.trade (Order.Taker (Order.limit Bid (Time 0) (toAmount 1) BTC (toPrice 30) USD))
          Exchange.balance

        balance `shouldBe` toAmount 80

        balance <- Exchange.runWith book $ do
          Exchange.trade (Order.Taker (Order.limit Ask (Time 0) (toAmount 1) BTC (toPrice 5) USD))
          Exchange.balance

        balance `shouldBe` toAmount 10


    context "when a limit order isn't fully matched" $ do

      let
        book =
          foldr Book.newOrder Book.empty
            [
              Order.Maker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 10) USD)
            , Order.Maker (Order.limit Ask (Time 0) (toAmount 2) BTC (toPrice 20) USD)
            ]

      it "should place the rest of the order on the correct side" $ do

        let
          bid =
            Order.Taker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 15) USD)

        book' <- Exchange.runWith book $ do
          Exchange.deposit 100
          Exchange.trade bid
          Exchange.orderbook
        book' `shouldBe` foldr Book.newOrder book [Order.toMaker bid]

        let
          ask =
            Order.Taker (Order.limit Ask (Time 0) (toAmount 2) BTC (toPrice 15) USD)

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
              Order.Maker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 10) USD)
            , Order.Maker (Order.limit Ask (Time 0) (toAmount 2) BTC (toPrice 20) USD)
            ]

      it "should be removed" $ do
        let
          maker =
            Order.Maker (Order.limit Ask (Time 0) (toAmount 2) BTC (toPrice 20) USD)

        book' <- Exchange.runWith book $ do
          Exchange.cancel maker
          Exchange.orderbook

        Book.bids book' `shouldBe`
          [
            Order.Maker (Order.limit Bid (Time 0) (toAmount 2) BTC (toPrice 10) USD)
          ]

