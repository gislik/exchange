module Main where

import qualified System.Posix.Signals as Signal
import qualified Control.Concurrent as Thread
import qualified Exchange.Asset as Asset
import qualified Exchange.Order as Order
import qualified Exchange.Book  as Book
import qualified Exchange
import Data.Typeable (Typeable)
import Data.Functor (void)
import Control.Exception (throwTo)
import Control.Monad (forM_, forever)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import System.Exit (ExitCode(ExitSuccess), exitSuccess)
import System.IO (hFlush, stdout)
import Exchange.Book (Book)
import Exchange.Asset 
import Exchange (Exchange)
import Exchange.Type
import Command

main :: IO ()
main = do
  handleKeyboardSignal
  Exchange.runWith orderbook $ do
    forever $ do
      command <- liftIO $ do
        newline
        putStr $ "Enter command: "
        hFlush stdout
        getCommand <* newline
      handleCommand command `catchError` (liftIO . putStrLn)
      return ()

handleCommand :: (Show a, Show b, Typeable a, Eq a, Typeable b, Eq b) 
  => Command a b -> Exchange a b IO ()
handleCommand command = do
  case command of
    Book _ _ -> do
      book <- Exchange.orderbook
      liftIO $ do
        Book.print book
    Order taker -> do
      trades <- Exchange.trade taker
      liftIO $ do
        putStrLn "Trades"
        putStrLn "------"
        forM_ trades print 
        putStrLn "------"
    Cancel maker -> do
      Exchange.cancel maker
    Clock -> do
      time <- Exchange.clock
      liftIO $ print time
    Blotter _-> do
      trades <- Exchange.blotter
      liftIO $ do
        putStrLn "Blotter"
        putStrLn "------"
        forM_ trades print 
        putStrLn "------"
    Balance -> do
      balance <- Exchange.balance
      liftIO $ do
        putStrLn $ "Balance: " ++ show balance
    Deposit amount -> do
      Exchange.deposit amount
      handleCommand Balance
    Withdraw amount -> do
      Exchange.withdraw amount
      handleCommand Balance
    ParseError err ->
      liftIO $ do
        putStrLn err
    Unknown ->
      liftIO $ do
        putStrLn "unknown command"
    Exit ->
      liftIO $ exitSuccess

orderbook :: Book Asset.BTC Asset.USD
orderbook = 
  foldr Book.newOrder Book.empty
    [
      Order.Maker (Order.limit Ask (Time 3) (toAmount 50) BTC (toPrice 102) USD)
    , Order.Maker (Order.limit Ask (Time 2) (toAmount 30) BTC (toPrice 102) USD)
    , Order.Maker (Order.limit Ask (Time 3) (toAmount 10) BTC (toPrice 101) USD)
    , Order.Maker (Order.limit Ask (Time 1) (toAmount 10) BTC (toPrice 101) USD)
    , Order.Maker (Order.limit Bid (Time 1) (toAmount 10) BTC (toPrice 99) USD)
    , Order.Maker (Order.limit Bid (Time 2) (toAmount 20) BTC (toPrice 98) USD)
    , Order.Maker (Order.limit Bid (Time 3) (toAmount 30) BTC (toPrice 97) USD)
    ]


--- signals
--
handleKeyboardSignal :: IO ()
handleKeyboardSignal = do
  tid <- Thread.myThreadId
  void $ Signal.installHandler
    Signal.keyboardSignal
    (Signal.Catch $ Thread.killThread tid >> throwTo tid ExitSuccess)
    Nothing
