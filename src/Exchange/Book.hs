{-# LANGUAGE DeriveFoldable #-}
module Exchange.Book where 

import qualified Data.List as List
import qualified Exchange.Order as Order
import Data.Typeable (Typeable, typeOf)
import Data.Ord (Down(Down), comparing)
import Exchange.Trade (Trade)
import Exchange.Entry

-- Book
data Book a b = Book {
    bids :: [Order.Maker a b]
  , asks :: [Order.Maker a b]
} deriving (Show, Typeable, Eq)

instance Semigroup (Book a b) where
  Book bids1 asks1 <> Book bids2 asks2 =
    Book (bids1 <> bids2) (asks1 <> asks2)

instance Monoid (Book a b) where
  mempty = Book [] []

instance Foldable (Book a) where
  foldr f x0 book = 
    foldr f x0 (quoteOf <$> bids book ++ asks book)

empty :: Book a b
empty = 
  Book [] []

newOrder :: Order.Maker a b -> Book a b -> Book a b
newOrder order book | Order.isBid order = 
  book { bids = List.insertBy (comparing (Down . priceOf)) order (bids book) }
newOrder order book | otherwise = 
  book { asks = List.insertBy (comparing priceOf) order (asks book) }

trade :: Order.Taker a b -> Book a b -> (Book a b, [Trade a b])
trade taker book = 
  let
    makers = 
      if Order.isBid taker 
        then asks book
        else bids book
    (makers', trades') =
      Order.trade taker makers 
    (bids', asks') =
      Order.splitSides makers'
    book' =
      if Order.isBid taker
        then book { bids = bids' ++ (bids book), asks = asks' }
        else book { bids = bids', asks = asks' ++ (asks book) }
  in
    (book', trades')

cancel :: (Eq a, Eq b) => Order.Maker a b -> Book a b -> Book a b
cancel maker book =
  book {
    bids = Order.cancel maker (bids book)
  , asks = Order.cancel maker (asks book)
  }

print :: (Show a, Show b, Typeable a, Typeable b) => Book a b -> IO ()
print book = do
  let 
    typeOfBook = show (typeOf book)
  putStrLn typeOfBook
  putStrLn (replicate (length typeOfBook) '=')
  mapM_ Order.print $ 
    Order.sumAmount <$> Order.groupBy priceOf (reverse $ asks book)
  putStrLn ""
  mapM_ Order.print $ 
    Order.sumAmount <$> Order.groupBy priceOf (bids book)
  putStrLn ""

