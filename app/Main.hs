{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Control.Exception (catch, SomeException)
import System.IO (getLine, hFlush, stdout)
import GHC.Read (Read, readPrec)
import Exchange

instance Read asset => Read (Order asset) where
  readPrec =
    Order <$> 
      readPrec <*> 
      readPrec <*> 
      pure (Time 0) <*> 
      (Amount <$> readPrec) <*> 
      (Price <$> readPrec)

book :: Book BTC
book = 
  foldr newOrder emptyBook
    [
      Maker (Order Ask BTC (Time 3) (Amount 50) (Price 102))
    , Maker (Order Ask BTC (Time 2) (Amount 30) (Price 102))
    , Maker (Order Ask BTC (Time 3) (Amount 10) (Price 101))
    , Maker (Order Ask BTC (Time 1) (Amount 10) (Price 101))
    , Maker (Order Bid BTC (Time 1) (Amount 10) (Price 99))
    , Maker (Order Bid BTC (Time 2) (Amount 20) (Price 98))
    , Maker (Order Bid BTC (Time 3) (Amount 30) (Price 97))
    ]


main :: IO ()
main = do
  runWith book $ do
    forM_ [1..] $ \i -> do
      book' <- orderbook
      order <- liftIO $ do
        printBook book'
        putStr "Enter trade: "
        hFlush stdout
        readLn `catch` parseErrorHandler
      trades <- place order 
      liftIO $ do
        putStrLn ""
        putStrLn "Trades"
        putStrLn "------"
        forM_ trades print 
        putStrLn "------"
        putStrLn ""
      return ()

parseErrorHandler :: SomeException -> IO (Order BTC)
parseErrorHandler e =  do
  putStrLn "no order"
  return (Order Bid BTC (Time 0) (Amount 0) (Price 0))
