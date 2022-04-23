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
import Exchange.Book (Book)
import Exchange.Entry
import Exchange.Type


-- Exchange State
data ExchangeState asset =
  ExchangeState
  {
    stateBookOf :: Book asset
  , stateTimeOf :: Time
  }

instance Semigroup (ExchangeState asset) where
  ExchangeState book1 time1 <> ExchangeState book2 time2 =
    ExchangeState (book1 <> book2) (time1 <> time2)

instance Monoid (ExchangeState asset) where
  mempty = ExchangeState Book.empty 0 

modifyBook :: (Book asset -> Book asset) -> ExchangeState asset -> ExchangeState asset
modifyBook f state = 
  state { stateBookOf = f (stateBookOf state) }

modifyTime :: (Time -> Time) -> ExchangeState asset -> ExchangeState asset
modifyTime f state =
  state { stateTimeOf = f (stateTimeOf state) }

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

trade :: Monad m => Order.Taker asset -> Exchange asset m [Trade asset]
trade order = do
  book <- State.gets stateBookOf
  time <- State.gets stateTimeOf
  let
    (book', trades) = 
      Book.trade (setTimeOf order time) book
  State.modify $ 
    modifyBook (const book') .
    modifyTime (+1) 
  return trades

cancel :: Eq asset => Monad m => Order.Maker asset -> Exchange asset m ()
cancel maker = 
  State.modify $ 
    modifyBook (Book.cancel maker) .
    modifyTime (+1)

orderbook :: Monad m => Exchange asset m (Book asset)
orderbook =
  State.gets stateBookOf

clock :: Monad m => Exchange asset m Time
clock = 
  State.gets stateTimeOf
