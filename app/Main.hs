module Main where

import qualified System.Posix.Signals as Signal
import qualified Control.Concurrent as Thread
import qualified Exchange.Asset as Asset
import qualified Exchange.Order as Order
import qualified Exchange.Book  as Book
import Data.Functor (void)
import Control.Exception (throwTo)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_, forever)
import Control.Exception (catch, SomeException)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (hFlush, stdout)
import GHC.Read (readPrec)
import Exchange
import Exchange.Order (Order)
import Exchange.Book (Book)

main :: IO ()
main = do
  handleKeyboardSignal
  runWith book $ do
    forever $ do
      book' <- orderbook
      time <- clock
      order <- liftIO $ do
        Book.print book'
        putStr $ show time
        putStr " => Enter trade: "
        hFlush stdout
        getOrder  <$> readLn `catch` parseErrorHandler
      trades <- trade (Order.Taker order)
      liftIO $ do
        putStrLn ""
        putStrLn "Trades"
        putStrLn "------"
        forM_ trades print 
        putStrLn "------"
        putStrLn ""
      return ()

parseErrorHandler :: SomeException -> IO (ReadOrder Asset.BTC)
parseErrorHandler _ =  do
  putStrLn "no order"
  return $ ReadOrder (Order.limit Bid Asset.BTC mempty mempty mempty)

newtype ReadOrder asset =
  ReadOrder {
    getOrder :: Order asset
  }

instance Read asset => Read (ReadOrder asset) where
  readPrec =
    let
      order =
        Order.limit <$> 
          readPrec <*> 
          readPrec <*> 
          pure mempty <*> 
          (Amount <$> readPrec) <*> 
          (Price <$> readPrec)
    in
      ReadOrder <$> order

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


--- signals

handleKeyboardSignal :: IO ()
handleKeyboardSignal = do
  tid <- Thread.myThreadId
  void $ Signal.installHandler
    Signal.keyboardSignal
    (Signal.Catch $ Thread.killThread tid >> throwTo tid ExitSuccess)
    Nothing
