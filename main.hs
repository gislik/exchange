{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Exchange where 

import Data.Monoid (Sum(..))
import Data.Foldable (mapM_, foldMap)
import Data.Typeable (Typeable, typeOf)
import Data.List (insertBy, groupBy, nubBy)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty as List (NonEmpty)
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

instance Semigroup Amount where
  Amount d <> Amount e = Amount (d + e)
  
instance Monoid Amount where
  mempty = Amount 0.0

-- Price
newtype Price = 
  Price Double 
    deriving (Show, Eq, Ord, Num)

-- Time
newtype Time = 
  Time Int
    deriving (Show, Eq)

-- Order
data Order asset = 
    Bid asset Time Amount Price
  | Ask asset Time Amount Price
  deriving (Show, Eq)


amountOf :: Order asset -> Amount
amountOf (Bid _ _ amount _) = amount
amountOf (Ask _ _ amount _) = amount

priceOf :: Order asset -> Price
priceOf (Bid _ _ _ price) = price
priceOf (Ask _ _ _ price) = price

timeOf :: Order asset -> Time
timeOf (Bid _ time _ _) = time
timeOf (Ask _ time _ _) = time

typeOfOrder :: Order asset -> String
typeOfOrder (Bid _ _ _ _) = "Bid"
typeOfOrder (Ask _ _ _ _) = "Ask"


printOrder :: Typeable asset => Order asset -> IO ()
printOrder order = do
  putStr $ typeOfOrder order 
  putStr $ " (" ++ show (priceOf order) ++ ")"
  putStr $ " (" ++ show (amountOf order) ++ ")"
  putStrLn ""

sumOrderAmount :: List.NonEmpty (Order asset) -> Order asset
sumOrderAmount orders = 
  let
    order = NonEmpty.head orders
    amount = foldMap amountOf orders
  in
    case order of
      Bid asset time _  price -> Bid asset time amount price
      Ask asset time _  price -> Ask asset time amount price

groupOrdersBy :: Eq b => (Order asset -> b) -> [Order asset] -> [List.NonEmpty (Order asset)]
groupOrdersBy f orders = 
  NonEmpty.fromList <$> groupBy (equalOn f) orders

equalOn :: Eq b => (a -> b) -> a -> a -> Bool
equalOn f = (==) `on` f

-- Book
data Book asset = Book {
    bids :: [Order asset]
  , asks :: [Order asset]
} deriving (Show, Typeable)


printBook :: (Show asset, Typeable asset) => Book asset -> IO ()
printBook book = do
  let typeOfBook = show (typeOf book)
  putStrLn typeOfBook
  putStrLn (replicate (length typeOfBook) '=')
  mapM_ printOrder $ 
    sumOrderAmount <$> groupOrdersBy priceOf (asks book)
  putStrLn ""
  mapM_ printOrder $ 
    sumOrderAmount <$> groupOrdersBy priceOf (reverse (bids book))
  putStrLn ""

emptyBook :: Book asset
emptyBook = Book [] []

newOrder :: Order asset -> Book asset -> Book asset
newOrder order@(Bid _ _ _ _) book = 
  book { bids = insertBy (comparing priceOf) order (bids book) }
newOrder order@(Ask _ _ _ _) book = 
  book { asks = insertBy (comparing (Down . priceOf)) order (asks book) }


bid1 = Bid BTC (Time 0) (Amount 1.0) (Price 1000)
bid2 = Bid BTC (Time 0) (Amount 0.5) (Price 1100)
bid3 = Bid BTC (Time 0) (Amount 0.6) (Price 900)
ask1 = Ask BTC (Time 0) (Amount 0.5) (Price 1500)
ask2 = Ask BTC (Time 0) (Amount 0.2) (Price 1500)
ask3 = Ask BTC (Time 0) (Amount 0.1) (Price 1400)

book = foldr newOrder emptyBook [bid1, bid2, ask1, ask2, ask3]



