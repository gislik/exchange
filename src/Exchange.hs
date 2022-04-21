module Exchange (
  module Exchange
, module Exchange.Entry
, module Exchange.Type
) where 

import qualified Control.Monad.Trans.State.Strict as State
import qualified Exchange.Order as Order
import qualified Exchange.Book  as Book
import Exchange.Order (Order)
import Control.Monad.Trans.State.Strict (StateT)
import Exchange.Trade (Trade)
import Exchange.Entry
import Exchange.Book (Book)
import Exchange.Type


-- Exchange
type Exchange asset m = 
  StateT (Book asset) m 

runWith :: Monad m => Book asset -> Exchange asset m a -> m a
runWith book ex = 
  State.evalStateT ex book

run :: Monad m => Exchange asset m a -> m a
run = 
  runWith Book.empty

empty :: Monad m => Exchange asset m ()
empty = 
  return ()


place :: Monad m => Order asset -> Exchange asset m [Trade asset]
place order = do
  book <- State.get 
  if Order.isBid order
    then do
      let (as, ts) = Order.trade (Book.asks book) (Order.Taker order)
      State.put $ book { Book.asks = as }
      return ts
    else do
      let (bs, ts) = Order.trade (Book.bids book) (Order.Taker order)
      State.put $ book { Book.bids = bs }
      return ts

orderbook :: Monad m => Exchange asset m (Book asset)
orderbook =
  State.get
