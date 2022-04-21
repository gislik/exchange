module Main where

import qualified Exchange.Asset as Asset
import qualified Exchange.Order as Order
import qualified Exchange.Book  as Book
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Control.Exception (catch, SomeException)
import System.IO (hFlush, stdout)
import GHC.Read (readPrec)
import Exchange
import Exchange.Order (Order)
import Exchange.Book (Book)


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
          pure (Time 0) <*> 
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


main :: IO ()
main = do
  runWith book $ do
    forM_ [Time 1..] $ \time -> do
      book' <- orderbook
      order <- liftIO $ do
        Book.print book'
        putStr "Enter trade: "
        hFlush stdout
        setTimeOf time . getOrder <$> readLn `catch` parseErrorHandler
      trades <- place order 
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
  return $ ReadOrder (Order.limit Bid Asset.BTC (Time 0) (Amount 0) (Price 0))
