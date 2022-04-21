module Exchange.Book where 

import qualified Data.List as List
import qualified Exchange.Order as Order
import Data.Typeable (Typeable, typeOf)
import Data.Ord (Down(Down), comparing)
import Exchange.Entry

-- Book
data Book asset = Book {
    bids :: [Order.Maker asset]
  , asks :: [Order.Maker asset]
} deriving (Show, Typeable)

instance Foldable Book where
  foldr f x0 book = 
    foldr f x0 (assetOf <$> bids book ++ asks book)

print :: (Show asset, Typeable asset) => Book asset -> IO ()
print book = do
  let typeOfBook = show (typeOf book)
  putStrLn typeOfBook
  putStrLn (replicate (length typeOfBook) '=')
  mapM_ Order.print $ 
    Order.sumAmount <$> Order.groupBy priceOf (reverse $ asks book)
  putStrLn ""
  mapM_ Order.print $ 
    Order.sumAmount <$> Order.groupBy priceOf (bids book)
  putStrLn ""

empty :: Book asset
empty = 
  Book [] []

newOrder :: Order.Maker asset -> Book asset -> Book asset
newOrder order book | Order.isBid order = 
  book { bids = List.insertBy (comparing (Down . priceOf)) order (bids book) }
newOrder order book | otherwise = 
  book { asks = List.insertBy (comparing priceOf) order (asks book) }



