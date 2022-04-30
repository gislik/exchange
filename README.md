# Exchange

This repo contains a toy implementation of a crypto exchange. The main purpose is to understand order books thoroughly and how best to implement them in [Haskell](https://www.haskell.org).

## Orders

The trade intent is expressed in an order. Orders are either bid (buy) orders or ask (sell) orders and they contain information about the amount of the base asset which the order giver is willing to exchange for a certain amount of the quote asset, as measured by the price.

~~~haskell
data Order a b =
  Order {
    orderBaseOf :: a          -- base asset
  , orderQuoteOf :: b         -- quote asset
  , orderSideOf :: Side       -- bid or ask
  , orderTimeOf :: Time
  , orderAmountOf :: Amount a -- amount of the base asset
  , orderPriceOf :: Price b   -- price measured in the quote asset
  , orderStyleOf :: Style     -- limit ordre or all-or-nothing
  }
    deriving (Eq, Typeable)
~~~

Currently there is a support two kind of orders.

1. Limit orders
2. All or Nothing (AON)

## Order book

Orders are managed by an order book. The order book is essentially a list of all the bids and the asks.

~~~haskell
data Book a b =
  Book {
    bids :: [Order.Maker a b]
  , asks :: [Order.Maker a b]
  }
    deriving (Show, Typeable, Eq)
~~~

## Trades

Two orders of the opposite site which match in price result in a trade.

~~~haskell
data Trade a b =
  Trade {
    tradeBaseOf :: a 
  , tradeQuoteOf :: b
  , tradeTimeOf :: Time 
  , tradeAmountOf :: Amount a
  , tradePriceOf :: Price b
  } 
    deriving (Eq)
~~~

## Engine

The engine is responsible for the order book, which it manages as a part of its state.

~~~haskell
type Engine a b m =
  ExceptT Error (StateT (EngineState a b) m)

data EngineState a b =
  EngineState {
    stateBookOf  :: Book a b
  , stateTimeOf  :: Time
  , stateTrades  :: [Trade a b]
  , stateBalance :: Amount b
  }
~~~

## Exchange

Finally the exchange embeds the engine and exposes a monad interface.

~~~haskell
-- Exchange
newtype Exchange a b m c =
  Exchange (Engine a b m c)
    deriving (
      Functor
    , Applicative
    , Alternative
    , Monad
    , MonadIO
    , MonadState (EngineState a b)
    , MonadError Error
    )
~~~
