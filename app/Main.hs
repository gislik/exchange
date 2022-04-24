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
import Exchange (Exchange)
import Exchange.Type
import Command

main :: IO ()
main = do
  handleKeyboardSignal
  Exchange.runWith orderbook $ do
    forever $ do
      time <- Exchange.clock
      command <- liftIO $ do
        newline
        putStr $ show time
        putStr " => Enter command: "
        hFlush stdout
        readLn <* newline
      handleCommand command `catchError` (liftIO . putStrLn)
      return ()

handleCommand :: (Show asset, Typeable asset, Eq asset) 
  => Command asset -> Exchange asset IO ()
handleCommand command = do
  case command of
    Book _ -> do
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
    Unknown ->
      liftIO $ do
        putStrLn "unknown command"
    Exit ->
      liftIO $ exitSuccess

orderbook :: Book Asset.BTC
orderbook = 
  foldr Book.newOrder Book.empty
    [
      Order.Maker (Order.limit Ask Asset.BTC (Time 3) (Amount 50) (Price 102))
    , Order.Maker (Order.limit Ask Asset.BTC (Time 2) (Amount 30) (Price 102))
    , Order.Maker (Order.limit Ask Asset.BTC (Time 3) (Amount 10) (Price 101))
    , Order.Maker (Order.limit Ask Asset.BTC (Time 1) (Amount 10) (Price 101))
    , Order.Maker (Order.limit Bid Asset.BTC (Time 1) (Amount 10) (Price 99))
    , Order.Maker (Order.limit Bid Asset.BTC (Time 2) (Amount 20) (Price 98))
    , Order.Maker (Order.limit Bid Asset.BTC (Time 3) (Amount 30) (Price 97))
    ]

newline :: IO ()
newline =
  putStrLn ""

--- signals
--
handleKeyboardSignal :: IO ()
handleKeyboardSignal = do
  tid <- Thread.myThreadId
  void $ Signal.installHandler
    Signal.keyboardSignal
    (Signal.Catch $ Thread.killThread tid >> throwTo tid ExitSuccess)
    Nothing
