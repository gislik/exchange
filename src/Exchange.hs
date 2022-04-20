{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Exchange where 

import Data.Monoid (Sum(..))
import Data.Foldable (mapM_, foldMap)
import Data.Typeable (Typeable, typeOf)
import Data.List (insertBy, groupBy, mapAccumL)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty as List (NonEmpty)
import Data.Function (on)
import Data.Ord (Down(Down), comparing)

import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.Trans.State.Strict (State)
import Control.Monad (forM)

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
    deriving (Show, Eq, Ord, Num)

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

isBid :: Order asset -> Bool
isBid order =
  case order of
    Bid _ _ _ _ -> True
    otherwise   -> False

isAsk :: Order asset -> Bool
isAsk order =
  case order of
    Ask _ _ _ _ -> True
    otherwise   -> False

assetOf :: Order asset -> asset
assetOf (Bid asset _ _ _) = asset
assetOf (Ask asset _ _ _) = asset

timeOf :: Order asset -> Time
timeOf (Bid _ time _ _) = time
timeOf (Ask _ time _ _) = time

amountOf :: Order asset -> Amount
amountOf (Bid _ _ amount _) = amount
amountOf (Ask _ _ amount _) = amount

priceOf :: Order asset -> Price
priceOf (Bid _ _ _ price) = price
priceOf (Ask _ _ _ price) = price

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
  book { bids = insertBy (comparing (Down . priceOf)) order (bids book) }
newOrder order@(Ask _ _ _ _) book = 
  book { asks = insertBy (comparing priceOf) order (asks book) }


bid1 = Bid BTC (Time 0) (Amount 1.0) (Price 1000)
bid2 = Bid BTC (Time 0) (Amount 0.5) (Price 1100)
bid3 = Bid BTC (Time 0) (Amount 0.6) (Price 900)
ask1 = Ask BTC (Time 0) (Amount 0.5) (Price 1500)
ask2 = Ask BTC (Time 0) (Amount 0.2) (Price 1500)
ask3 = Ask BTC (Time 0) (Amount 0.1) (Price 1400)

bid' = Bid BTC (Time 0) (Amount 0.6) (Price 1500)
ask' = Ask BTC (Time 0) (Amount 0.6) (Price 1000)

book = foldr newOrder emptyBook [bid1, bid2, ask1, ask2, ask3]


-- Exchange
type Exchange asset = 
  State (Book asset) 

runExchange :: Book asset -> Exchange asset a -> a
runExchange book ex = State.evalState ex book

printExchange :: Exchange asset ()
printExchange = undefined


emptyExchange :: Exchange asset ()
emptyExchange = 
  return ()


newExchange :: Book asset -> Exchange asset ()
newExchange book = do
  State.put book
  emptyExchange

data Trade asset =
  Trade asset Time Amount Price
    deriving (Show)

tradeAmountOf :: Trade asset -> Amount
tradeAmountOf (Trade _ _ amount _) = amount

placeOrder :: Order asset -> Exchange asset [Trade asset]
placeOrder order = do
  book <- State.get 
  if isBid order
    then do
      let (ts, as) = mapAccumL (matchOrder order) [] (asks book)
      State.put $ book { asks = as }
      return ts
    else do
      let (ts, bs) = mapAccumL (matchOrder order) [] (bids book)
      State.put $ book { bids = bs }
      return ts


compareOrder :: Order asset -> Order asset -> Maybe (Trade asset)
compareOrder taker maker = 
  let
    asset = assetOf taker
    time = timeOf taker
    amount = min (amountOf taker) (amountOf maker)
    price = priceOf maker
  in
    if isBid taker && isAsk maker && priceOf taker >= priceOf maker && amount > 0
      then Just (Trade asset time amount price)
    else if isAsk taker && isBid maker && priceOf taker <= priceOf maker && amount > 0
      then Just (Trade asset time amount price)
    else 
      Nothing
    

matchOrder :: Order asset -> [Trade asset] -> Order asset ->  ([Trade asset], Order asset)
matchOrder order ts maker = 
  let
    decreaseAmount order amount' = 
      case order of
        Bid asset time amount price -> Bid asset time (amount-amount') price
        Ask asset time amount price -> Ask asset time (amount-amount') price
    totalTraded = sum $ tradeAmountOf <$> ts
    decreasedOrder = decreaseAmount order totalTraded
  in
    case compareOrder decreasedOrder maker of
      Nothing -> (ts, maker)
      Just td -> (td:ts, decreaseAmount maker (tradeAmountOf td))
