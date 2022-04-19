module Exchange where 

import Data.Foldable (mapM_)
import Data.Typeable (Typeable, typeOf)
import Data.List (insertBy)
import Data.Function (on)
import Data.Ord (Down(Down), comparing)

-- Assets
data ETH = 
  ETH   
    deriving (Show)

data BTC = 
  BTC 
    deriving (Show)


-- Amount
newtype Amount = 
  Amount Double 
    deriving (Show, Eq)

-- Price
newtype Price = 
  Price Double 
    deriving (Show, Eq, Ord)

-- Time
newtype Time = 
  Time Int

-- Order
data Order asset = 
    Bid asset Amount Price
  | Ask asset Amount Price
  deriving (Show, Eq)

amountOf :: Order asset -> Amount
amountOf (Bid _ amount _) = amount
amountOf (Ask _ amount _) = amount

priceOf :: Order asset -> Price
priceOf (Bid _ _ price) = price
priceOf (Ask _ _ price) = price

typeOfOrder :: Order asset -> String
typeOfOrder (Bid _ _ _) = "Bid"
typeOfOrder (Ask _ _ _) = "Ask"


printOrder :: Typeable asset => Order asset -> IO ()
printOrder order = do
  putStr $ typeOfOrder order 
  putStr $ " (" ++ show (priceOf order) ++ ")"
  putStr $ " (" ++ show (amountOf order) ++ ")"
  putStrLn ""

-- Book
data Book asset = Book {
    bids :: [Order asset]
  , asks :: [Order asset]
} deriving (Show, Typeable)


printBook :: (Show asset, Typeable asset) => Book asset -> IO ()
printBook book = do
  let typeOfBook = show (typeOf book)
  putStrLn typeOfBook
  -- putStrLn (take (length typeOfBook) (repeat '='))
  putStrLn (replicate (length typeOfBook) '=')
  mapM_ printOrder (asks book)
  putStrLn ""
  mapM_ printOrder (reverse (bids book))
  putStrLn ""

-- instance Semigroup (Book asset) where
--   (Book bs1 as1) <> (Book bs2 as2) = Book (bs1 <> bs2) (as1 <> as2)
-- 
-- instance Monoid (Book asset) where
--   mempty = emptyBook
--   mappend = (<>)

emptyBook :: Book asset
emptyBook = Book [] []

newOrder :: Order asset -> Book asset -> Book asset
newOrder order@(Bid _ _ _) book = 
  book { bids = insertBy (comparing priceOf) order (bids book) }
newOrder order@(Ask _ _ _) book = 
  book { asks = insertBy (comparing (Down . priceOf)) order (asks book) }

bid1 = Bid BTC (Amount 1) (Price 1000)
bid2 = Bid BTC (Amount 0.5) (Price 1100)
bid3 = Bid BTC (Amount 0.6) (Price 900)
ask1 = Ask BTC (Amount 0.5) (Price 1500)
ask2 = Ask BTC (Amount 0.2) (Price 1500)
ask3 = Ask BTC (Amount 0.1) (Price 1400)

book = foldr newOrder emptyBook [bid1, bid2, ask1, ask2, ask3]


