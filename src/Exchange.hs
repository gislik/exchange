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
import Control.Monad (when)
import Control.Monad.State.Strict (StateT, MonadState, MonadIO)
import Control.Monad.Except (ExceptT, MonadError)
import Exchange.Trade (Trade)
import Exchange.Book (Book)
import Exchange.Entry
import Exchange.Type

-- Engine State
data EngineState a b =
  EngineState
  {
    stateBookOf  :: Book a b
  , stateTimeOf  :: Time
  , stateTrades  :: [Trade a b]
  , stateBalance :: Amount b
  }

instance Semigroup (EngineState a b) where
  EngineState book1 time1 trades1 balance1 <> EngineState book2 time2 trades2 balance2 =
    EngineState (book1 <> book2) (time1 <> time2) (trades1 ++ trades2) (balance1+balance2)

instance Monoid (EngineState a b) where
  mempty = EngineState Book.empty 0 [] 0

modifyBook :: (Book a b -> Book a b) -> EngineState a b -> EngineState a b
modifyBook f state =
  state { stateBookOf = f (stateBookOf state) }

modifyTime :: (Time -> Time) -> EngineState a b -> EngineState a b
modifyTime f state =
  state { stateTimeOf = f (stateTimeOf state) }

modifyTrades :: ([Trade a b] -> [Trade a b]) -> EngineState a b -> EngineState a b
modifyTrades f state =
  state { stateTrades = f (stateTrades state) }

modifyBalance :: (Amount b -> Amount b) -> EngineState a b -> EngineState a b
modifyBalance f state =
  state { stateBalance = f (stateBalance state) }

type Error = String

-- Engine
type Engine a b m =
  ExceptT Error (StateT (EngineState a b) m)

-- Exchange
newtype Exchange a b m c =
  Exchange (Engine a b m c)
  deriving
    (
      Functor
    , Applicative
    , Alternative
    , Monad
    , MonadIO
    , MonadState (EngineState a b)
    , MonadError Error
    )

runWith :: MonadFail m => Book a b -> Exchange a b m c -> m c
runWith book (Exchange engine) = do
  res <- State.evalStateT (Exception.runExceptT engine) (mempty { stateBookOf = book })
  case res of
    Left err -> fail err
    Right res' -> return res'

run :: MonadFail m => Exchange a b m c -> m c
run =
  runWith Book.empty

empty :: Monad m => Exchange a b m ()
empty =
  return ()

trade :: Monad m => Order.Taker a b -> Exchange a b m [Trade a b]
trade taker = do
  book <- State.gets stateBookOf
  time <- State.gets stateTimeOf
  bal <- Exchange.balance
  let
    (book', trades) =
      Book.trade (setTimeOf taker time) book
    cost =
      foldMap costOf trades
    op =
      if Order.isBid taker
        then (-)
        else (+)
  when (bal `op` cost < 0) $ do
    Exception.throwError "cost of trades greater than balance"
    return ()
  State.modify $
    modifyBook (const book') .
    modifyTime (+1) .
    modifyTrades (++trades) .
    modifyBalance (`op` cost)
  return trades


cancel :: (Eq a, Eq b) => Monad m => Order.Maker a b -> Exchange a b m ()
cancel maker =
  State.modify $
    modifyBook (Book.cancel maker) .
    modifyTime (+1)

blotter :: Monad m => Exchange a b m [Trade a b]
blotter =
  State.gets stateTrades

orderbook :: Monad m => Exchange a b m (Book a b)
orderbook =
  State.gets stateBookOf

clock :: Monad m => Exchange a b m Time
clock =
  State.gets stateTimeOf

balance :: Monad m => Exchange a b m (Amount b)
balance =
  State.gets stateBalance

deposit :: Monad m => Amount b -> Exchange a b m ()
deposit amount =
  State.modify $
    modifyBalance (+ amount)

withdraw :: Monad m => Amount b -> Exchange a b m ()
withdraw amount = do
  bal <- State.gets stateBalance
  when (amount > bal) $ do
    Exception.throwError "withdrawal amount greater than balance"
    return ()
  State.modify $
    modifyBalance (+ (-amount))
