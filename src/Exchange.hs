{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Exchange (
  module Exchange
, module Exchange.Entry
, module Exchange.Type
) where 

import qualified Control.Monad.Trans.State.Strict as State
import qualified Exchange.Order as Order
import qualified Exchange.Book  as Book
import Control.Monad.Trans.State.Strict (StateT)
import Exchange.Trade (Trade)
import Exchange.Order (Order)
import Exchange.Book (Book)
import Exchange.Entry
import Exchange.Type


-- Exchange State
data ExchangeState asset =
  ExchangeState
  {
    exchangeBookOf :: Book asset
  , exchangeTimeOf :: Time
  }

instance Semigroup (ExchangeState asset) where
  ExchangeState book1 time1 <> ExchangeState book2 time2 =
    ExchangeState (book1 <> book2) (time1 <> time2)

instance Monoid (ExchangeState asset) where
  mempty = ExchangeState Book.empty 0

modifyBook :: (Book asset -> Book asset) -> ExchangeState asset -> ExchangeState asset
modifyBook f state = 
  state { exchangeBookOf = f (exchangeBookOf state) }

modifyTime :: (Time -> Time) -> ExchangeState asset -> ExchangeState asset
modifyTime f state =
  state { exchangeTimeOf = f (exchangeTimeOf state) }


-- Exchange
type Exchange asset m = 
  StateT (ExchangeState asset) m 

runWith :: Monad m => Book asset -> Exchange asset m a -> m a
runWith book ex = 
  State.evalStateT ex (mempty { exchangeBookOf = book })

run :: Monad m => Exchange asset m a -> m a
run = 
  runWith Book.empty

empty :: Monad m => Exchange asset m ()
empty = 
  return ()

place :: Monad m => Order asset -> Exchange asset m [Trade asset]
place order = do
  book <- State.gets exchangeBookOf
  time <- State.gets exchangeTimeOf
  let
    order' =
      setTimeOf order time
  if Order.isBid order
    then do
      let (as, ts) = Order.trade (Book.asks book) (Order.Taker order')
      State.modify $ do
        modifyBook (\b -> b { Book.asks = as })
        modifyTime (+1) 
      return ts
    else do
      let (bs, ts) = Order.trade (Book.bids book) (Order.Taker order')
      State.modify $
        modifyBook (\b -> b { Book.bids = bs }) .
        modifyTime (+1)
      return ts

orderbook :: Monad m => Exchange asset m (Book asset)
orderbook =
  State.gets exchangeBookOf

clock :: Monad m => Exchange asset m Time
clock = 
  State.gets exchangeTimeOf
