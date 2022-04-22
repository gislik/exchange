module Exchange.Book where 

import qualified Data.List as List
import qualified Exchange.Order as Order
import Data.Typeable (Typeable, typeOf)
import Data.Ord (Down(Down), comparing)
import Exchange.Trade (Trade)
import Exchange.Entry

-- Book
data Book asset = Book {
    bids :: [Order.Maker asset]
  , asks :: [Order.Maker asset]
} deriving (Show, Typeable, Eq)

instance Semigroup (Book asset) where
  Book bids1 asks1 <> Book bids2 asks2 =
    Book (bids1 <> bids2) (asks1 <> asks2)

instance Monoid (Book asset) where
  mempty = Book [] []

instance Foldable Book where
  foldr f x0 book = 
    foldr f x0 (assetOf <$> bids book ++ asks book)

empty :: Book asset
empty = 
  Book [] []

newOrder :: Order.Maker asset -> Book asset -> Book asset
newOrder order book | Order.isBid order = 
  book { bids = List.insertBy (comparing (Down . priceOf)) order (bids book) }
newOrder order book | otherwise = 
  book { asks = List.insertBy (comparing priceOf) order (asks book) }

trade :: Order.Taker asset -> Book asset -> (Book asset, [Trade asset])
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

