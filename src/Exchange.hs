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
    stateBookOf :: Book asset
  , stateTimeOf :: Time
  , stateTrades :: [Trade asset]
  }

instance Semigroup (ExchangeState asset) where
  ExchangeState book1 time1 trades1 <> ExchangeState book2 time2 trades2 =
    ExchangeState (book1 <> book2) (time1 <> time2) (trades1 ++ trades2)

instance Monoid (ExchangeState asset) where
  mempty = ExchangeState Book.empty 0 []

modifyBook :: (Book asset -> Book asset) -> ExchangeState asset -> ExchangeState asset
modifyBook f state = 
  state { stateBookOf = f (stateBookOf state) }

modifyTime :: (Time -> Time) -> ExchangeState asset -> ExchangeState asset
modifyTime f state =
  state { stateTimeOf = f (stateTimeOf state) }

modifyTrades :: ([Trade asset] -> [Trade asset]) -> ExchangeState asset -> ExchangeState asset
modifyTrades f state =
  state { stateTrades = f (stateTrades state) }

clearTrades :: ExchangeState asset -> ExchangeState asset
clearTrades state = 
  state { stateTrades = [] }

-- Exchange
type Exchange asset m = 
  StateT (ExchangeState asset) m 

runWith :: Monad m => Book asset -> Exchange asset m a -> m a
runWith book ex = 
  State.evalStateT ex (mempty { stateBookOf = book })

run :: Monad m => Exchange asset m a -> m a
run = 
  runWith Book.empty

empty :: Monad m => Exchange asset m ()
empty = 
  return ()

place :: Monad m => Order asset -> Exchange asset m ()
place order = do
  book <- State.gets stateBookOf
  time <- State.gets stateTimeOf
  let
    order' =
      setTimeOf order time
  if Order.isBid order
    then do
      let (as, ts) = Order.trade (Book.asks book) (Order.Taker order')
      State.modify $ 
        modifyBook (\b -> b { Book.asks = as }) .
        modifyTrades (\ts' -> ts' ++ ts) .
        modifyTime (+1) 
    else do
      let (bs, ts) = Order.trade (Book.bids book) (Order.Taker order')
      State.modify $
        modifyBook (\b -> b { Book.bids = bs }) .
        modifyTrades (\ts' -> ts' ++ ts) .
        modifyTime (+1)

trades :: Monad m => Exchange asset m [Trade asset]
trades = do
  state <- State.get
  let 
    trades' = 
      stateTrades state
  State.put $ clearTrades state
  return trades'

orderbook :: Monad m => Exchange asset m (Book asset)
orderbook =
  State.gets stateBookOf

clock :: Monad m => Exchange asset m Time
clock = 
  State.gets stateTimeOf
