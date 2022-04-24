{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Exchange (
  module Exchange
, module Exchange.Entry
, module Exchange.Type
) where 

import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Except as Exception
import qualified Exchange.Order as Order
import qualified Exchange.Book  as Book
import Control.Applicative (Alternative)
import Control.Monad.State.Strict (StateT, MonadState, MonadIO)
import Control.Monad.Except (ExceptT, MonadError)
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
  , stateTrades :: [Trade asset]
  , stateBalance  :: Amount
  }

instance Semigroup (ExchangeState asset) where
  ExchangeState book1 time1 trades1 balance1 <> ExchangeState book2 time2 trades2 balance2 =
    ExchangeState (book1 <> book2) (time1 <> time2) (trades1 ++ trades2) (balance1+balance2)

instance Monoid (ExchangeState asset) where
  mempty = ExchangeState Book.empty 0 [] 0

modifyBook :: (Book asset -> Book asset) -> ExchangeState asset -> ExchangeState asset
modifyBook f state = 
  state { stateBookOf = f (stateBookOf state) }

modifyTime :: (Time -> Time) -> ExchangeState asset -> ExchangeState asset
modifyTime f state =
  state { stateTimeOf = f (stateTimeOf state) }

modifyTrades :: ([Trade asset] -> [Trade asset]) -> ExchangeState asset -> ExchangeState asset
modifyTrades f state =
  state { stateTrades = f (stateTrades state) }

modifyBalance :: (Amount -> Amount) -> ExchangeState asset -> ExchangeState asset
modifyBalance f state =
  state { stateBalance = f (stateBalance state) }

type Error = String

-- Engine
type Engine asset m =
  ExceptT Error (StateT (ExchangeState asset) m)

-- Exchange
newtype Exchange asset m a =
  Exchange (Engine asset m a)
  deriving 
    ( 
      Functor
    , Applicative
    , Alternative
    , Monad
    , MonadIO
    , MonadState (ExchangeState asset)
    , MonadError Error
    )

runWith :: MonadFail m => Book asset -> Exchange asset m a -> m a
runWith book (Exchange engine) = do
  res <- State.evalStateT (Exception.runExceptT engine) (mempty { stateBookOf = book }) 
  case res of
    Left err -> fail err
    Right res' -> return res'

run :: MonadFail m => Exchange asset m a -> m a
run = 
  runWith Book.empty

empty :: Monad m => Exchange asset m ()
empty = 
  return ()

trade :: Monad m => Order.Taker asset -> Exchange asset m [Trade asset]
trade order = do
  book <- State.gets stateBookOf
  time <- State.gets stateTimeOf
  bal <- balance
  let
    (book', trades) = 
      Book.trade (setTimeOf order time) book
    c =
      foldMap costOf trades
  if c <= (Price 1) `times` bal
    then do
      State.modify $
        modifyBook (const book') .
        modifyTime (+1) .
        modifyTrades (++trades)
      return trades
    else Exception.throwError "cost of trades greater than balance"


cancel :: Eq asset => Monad m => Order.Maker asset -> Exchange asset m ()
cancel maker = 
  State.modify $ 
    modifyBook (Book.cancel maker) .
    modifyTime (+1)

blotter :: Monad m => Exchange asset m [Trade asset]
blotter = 
  State.gets stateTrades

orderbook :: Monad m => Exchange asset m (Book asset)
orderbook =
  State.gets stateBookOf

clock :: Monad m => Exchange asset m Time
clock = 
  State.gets stateTimeOf

balance :: Monad m => Exchange asset m Amount
balance =
  State.gets stateBalance

deposit :: Monad m => Amount -> Exchange asset m ()
deposit amount =
  State.modify $
    modifyBalance (+amount)

withdraw :: Monad m => Amount -> Exchange asset m ()
withdraw amount = do
  bal <- State.gets stateBalance
  if amount <= bal
    then State.modify $
    modifyBalance (+(-amount))
  else Exception.throwError "withdrawal amount greater than balance"
