module Main where

import qualified System.Posix.Signals as Signal
import qualified Control.Concurrent as Thread
import qualified Exchange.Asset as Asset
import qualified Exchange.Order as Order
import qualified Exchange.Book  as Book
import Data.Typeable (Typeable)
import Data.Functor (void)
import Control.Exception (throwTo)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_, forever)
import Control.Exception (catch, SomeException)
import System.Exit (ExitCode(ExitSuccess), exitSuccess)
import System.IO (hFlush, stdout)
import Exchange
import Exchange.Book (Book)
import Command

main :: IO ()
main = do
  handleKeyboardSignal
  runWith book $ do
    forever $ do
      book' <- orderbook
      time <- clock
      command <- liftIO $ do
        Book.print book'
        putStr $ show time
        putStr " => Enter trade: "
        hFlush stdout
        readLn `catch` parseErrorHandler
      handleCommand command
      return ()

handleCommand :: (Show asset, Typeable asset) => Command asset -> Exchange asset IO ()
handleCommand command =
  case command of
    Order taker -> do
      trades <- trade taker
      liftIO $ do
        putStrLn ""
        putStrLn "Trades"
        putStrLn "------"
        forM_ trades print 
        putStrLn "------"
        putStrLn ""
    Cancel taker ->
      liftIO $ do
        putStr "cancel "
        print taker
        putStrLn ""
    Unknown ->
      liftIO $ do
        putStrLn "unknown command"
        putStrLn ""
    Exit ->
      liftIO $ exitSuccess

book :: Book Asset.BTC
book = 
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


parseErrorHandler :: SomeException -> IO (Command asset)
parseErrorHandler _ =  do
  return Unknown 

--- signals

handleKeyboardSignal :: IO ()
handleKeyboardSignal = do
  tid <- Thread.myThreadId
  void $ Signal.installHandler
    Signal.keyboardSignal
    (Signal.Catch $ Thread.killThread tid >> throwTo tid ExitSuccess)
    Nothing
