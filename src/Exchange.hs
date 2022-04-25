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

-- Exchange State
data ExchangeState base quote =
  ExchangeState
  {
    stateBookOf  :: Book base quote
  , stateTimeOf  :: Time
  , stateTrades  :: [Trade base quote]
  , stateBalance :: Amount quote
  }

instance Semigroup (ExchangeState base quote) where
  ExchangeState book1 time1 trades1 balance1 <> ExchangeState book2 time2 trades2 balance2 =
    ExchangeState (book1 <> book2) (time1 <> time2) (trades1 ++ trades2) (balance1+balance2)

instance Monoid (ExchangeState base quote) where
  mempty = ExchangeState Book.empty 0 [] 0

modifyBook :: (Book base quote -> Book base quote) -> ExchangeState base quote -> ExchangeState base quote
modifyBook f state =
  state { stateBookOf = f (stateBookOf state) }

modifyTime :: (Time -> Time) -> ExchangeState base quote -> ExchangeState base quote
modifyTime f state =
  state { stateTimeOf = f (stateTimeOf state) }

modifyTrades :: ([Trade base quote] -> [Trade base quote]) -> ExchangeState base quote -> ExchangeState base quote
modifyTrades f state =
  state { stateTrades = f (stateTrades state) }

modifyBalance :: (Amount quote -> Amount quote) -> ExchangeState base quote -> ExchangeState base quote
modifyBalance f state =
  state { stateBalance = f (stateBalance state) }

type Error = String

-- Engine
type Engine base quote  m =
  ExceptT Error (StateT (ExchangeState base quote) m)

-- Exchange
newtype Exchange base quote m a =
  Exchange (Engine base quote m a)
  deriving
    (
      Functor
    , Applicative
    , Alternative
    , Monad
    , MonadIO
    , MonadState (ExchangeState base quote)
    , MonadError Error
    )

runWith :: MonadFail m => Book base quote -> Exchange base quote m a -> m a
runWith book (Exchange engine) = do
  res <- State.evalStateT (Exception.runExceptT engine) (mempty { stateBookOf = book })
  case res of
    Left err -> fail err
    Right res' -> return res'

run :: MonadFail m => Exchange base quote m a -> m a
run =
  runWith Book.empty

empty :: Monad m => Exchange base quote m ()
empty =
  return ()

trade :: Monad m => Order.Taker base quote -> Exchange base quote m [Trade base quote]
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


cancel :: (Eq base, Eq quote) => Monad m => Order.Maker base quote -> Exchange base quote m ()
cancel maker =
  State.modify $
    modifyBook (Book.cancel maker) .
    modifyTime (+1)

blotter :: Monad m => Exchange base quote m [Trade base quote]
blotter =
  State.gets stateTrades

orderbook :: Monad m => Exchange base quote m (Book base quote)
orderbook =
  State.gets stateBookOf

clock :: Monad m => Exchange base quote m Time
clock =
  State.gets stateTimeOf

balance :: Monad m => Exchange base quote m (Amount quote)
balance =
  State.gets stateBalance

deposit :: Monad m => Amount quote -> Exchange base quote m ()
deposit amount =
  State.modify $
    modifyBalance (+ amount)

withdraw :: Monad m => Amount quote -> Exchange base quote m ()
withdraw amount = do
  bal <- State.gets stateBalance
  when (amount > bal) $ do
    Exception.throwError "withdrawal amount greater than balance"
    return ()
  State.modify $
    modifyBalance (+ (-amount))
