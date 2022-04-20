{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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
    deriving (Show, Eq)

data BTC = 
  BTC 
    deriving (Show, Eq)


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

data Side =
    Bid 
  | Ask
  deriving (Show, Eq)

-- Entry
class Entry a asset where
  sideOf   :: a asset -> Side
  assetOf  :: a asset -> asset
  timeOf   :: a asset -> Time
  amountOf :: a asset -> Amount
  priceOf  :: a asset -> Price


-- Trade
data Trade asset =
  Trade {
      tradeAssetOf :: asset 
    , tradeTimeOf :: Time 
    , tradeAmountOf :: Amount 
    , tradePriceOf :: Price
    }
  deriving (Show, Eq)

instance Entry Trade asset where
  -- sideOf = tradeSideOf
  sideOf   = const Bid -- TODO: hardcoded
  assetOf  = tradeAssetOf
  timeOf   = tradeTimeOf
  amountOf = tradeAmountOf
  priceOf  = tradePriceOf


-- Order
data Order asset = 
  Order {
      orderSideOf :: Side
    , orderAssetOf :: asset
    , orderTimeOf :: Time
    , orderAmountOf :: Amount
    , orderPriceOf :: Price
    } 
  deriving (Show, Eq)

instance Entry Order asset where
  sideOf   = orderSideOf
  assetOf  = orderAssetOf
  timeOf   = orderTimeOf
  amountOf = orderAmountOf
  priceOf  = orderPriceOf

isBid :: Entry a asset => a asset -> Bool
isBid order =
  case sideOf order of
    Bid       -> True
    otherwise -> False

isAsk :: Entry a asset => a asset -> Bool
isAsk order =
  case sideOf order of
    Ask       -> True
    otherwise -> False

newtype Maker asset = 
  Maker (Order asset)

instance Entry Maker asset where
  sideOf   (Maker order) = sideOf order
  assetOf  (Maker order) = assetOf order
  timeOf   (Maker order) = timeOf order
  amountOf (Maker order) = amountOf order
  priceOf  (Maker order) = priceOf order

newtype Taker asset = 
  Taker (Order asset)

instance Entry Taker asset where
  sideOf   (Taker order) = sideOf order
  assetOf  (Taker order) = assetOf order
  timeOf   (Taker order) = timeOf order
  amountOf (Taker order) = amountOf order
  priceOf  (Taker order) = priceOf order

matchOrders :: Maker asset -> Taker asset -> Maybe (Trade asset)
matchOrders maker taker = 
  let
    asset  = assetOf taker
    time   = timeOf taker
    amount = min (amountOf taker) (amountOf maker)
    price  = priceOf maker
  in
    if isBid taker && isAsk maker && priceOf taker >= priceOf maker && amount > 0
      then Just (Trade asset time amount price)
    else if isAsk taker && isBid maker && priceOf taker <= priceOf maker && amount > 0
      then Just (Trade asset time amount price)
    else 
      Nothing

needsNewName :: Order asset -> [Trade asset] -> Order asset ->  ([Trade asset], Order asset)
needsNewName order ts maker = 
  let
    decreaseAmount order amount' = 
        order { orderAmountOf = (amountOf order)-amount' }
    totalTraded = sum $ tradeAmountOf <$> ts
    decreasedOrder = decreaseAmount order totalTraded
  in
    case matchOrders (Maker maker) (Taker decreasedOrder) of
      Nothing -> (ts, maker)
      Just td -> (td:ts, decreaseAmount maker (tradeAmountOf td))

printOrder :: Typeable asset => Order asset -> IO ()
printOrder order = do
  putStr $ show (sideOf order)
  putStr $ " (" ++ show (priceOf order) ++ ")"
  putStr $ " (" ++ show (amountOf order) ++ ")"
  putStrLn ""

sumakerAmount :: List.NonEmpty (Order asset) -> Order asset
sumakerAmount orders = 
  let
    order  = NonEmpty.head orders
    amount = foldMap amountOf orders
  in
    order { orderAmountOf = amount }

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
    sumakerAmount <$> groupOrdersBy priceOf (asks book)
  putStrLn ""
  mapM_ printOrder $ 
    sumakerAmount <$> groupOrdersBy priceOf (reverse (bids book))
  putStrLn ""

emptyBook :: Book asset
emptyBook = Book [] []

newOrder :: Order asset -> Book asset -> Book asset
newOrder order book | isBid order = 
  book { bids = insertBy (comparing (Down . priceOf)) order (bids book) }
newOrder order book | isAsk order || otherwise = 
  book { asks = insertBy (comparing priceOf) order (asks book) }


-- bid1 = Order Bid BTC (Time 0) (Amount 1.0) (Price 1000)
-- bid2 = Order Bid BTC (Time 0) (Amount 0.5) (Price 1100)
-- bid3 = Order Bid BTC (Time 0) (Amount 0.6) (Price 900)
-- ask1 = Order Ask BTC (Time 0) (Amount 0.5) (Price 1500)
-- ask2 = Order Ask BTC (Time 0) (Amount 0.2) (Price 1500)
-- ask3 = Order Ask BTC (Time 0) (Amount 0.1) (Price 1400)

-- bid' = Order Bid BTC (Time 0) (Amount 0.6) (Price 1500)
-- ask' = Order Ask BTC (Time 0) (Amount 0.6) (Price 1000)

-- book = foldr newOrder emptyBook [bid1, bid2, ask1, ask2, ask3]


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


-- newExchange :: Book asset -> Exchange asset ()
-- newExchange book = do
  -- State.put book
  -- emptyExchange

placeOrder :: Order asset -> Exchange asset [Trade asset]
placeOrder order = do
  book <- State.get 
  if isBid order
    then do
      let (ts, as) = mapAccumL (needsNewName order) [] (asks book)
      State.put $ book { asks = as }
      return ts
    else do
      let (ts, bs) = mapAccumL (needsNewName order) [] (bids book)
      State.put $ book { bids = bs }
      return ts


