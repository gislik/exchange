{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Exchange where 

-- import Data.Monoid (Sum(..))
-- import Data.Foldable (mapM_, foldMap)
import Data.Typeable (Typeable, typeOf)
import Data.List (insertBy, groupBy, mapAccumL)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty as List (NonEmpty)
import Data.Function (on)
import Data.Ord (Down(Down), comparing)

import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.Trans.State.Strict (State)
-- import Control.Monad (forM)

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

  setAmountOf :: a asset -> Amount -> a asset


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
  setAmountOf trade amount = trade { tradeAmountOf = amount }


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
  setAmountOf order amount = order { orderAmountOf = amount }

isBid :: Entry a asset => a asset -> Bool
isBid order =
  case sideOf order of
    Bid -> True
    _   -> False

isAsk :: Entry a asset => a asset -> Bool
isAsk order =
  case sideOf order of
    Ask -> True
    _   -> False

newtype Maker asset = 
  Maker (Order asset)
    deriving (Show, Eq)

instance Entry Maker asset where
  sideOf   (Maker order) = sideOf order
  assetOf  (Maker order) = assetOf order
  timeOf   (Maker order) = timeOf order
  amountOf (Maker order) = amountOf order
  priceOf  (Maker order) = priceOf order
  setAmountOf (Maker order) amount = Maker order { orderAmountOf = amount }

newtype Taker asset = 
  Taker (Order asset)

instance Entry Taker asset where
  sideOf   (Taker order) = sideOf order
  assetOf  (Taker order) = assetOf order
  timeOf   (Taker order) = timeOf order
  amountOf (Taker order) = amountOf order
  priceOf  (Taker order) = priceOf order
  setAmountOf (Taker order) amount = Taker order { orderAmountOf = amount }

matchOrders :: Maker asset -> Taker asset -> Maybe (Trade asset)
matchOrders maker taker = 
  let
    asset  = assetOf taker
    time   = timeOf taker
    amount = min (amountOf taker) (amountOf maker)
    price  = priceOf maker
    trade  = Trade asset time amount price
  in
    if isBid taker && isAsk maker && priceOf taker >= priceOf maker && amount > 0
      then Just trade
    else if isAsk taker && isBid maker && priceOf taker <= priceOf maker && amount > 0
      then Just trade
    else 
      Nothing

tradeOrders :: [Maker asset] -> Taker asset -> ([Maker asset], [Trade asset])
tradeOrders makers taker = 
  let
    go maker trade' =
      case trade' of 
        Just trade | amountOf maker - amountOf trade > 0 -> 
          ([setAmountOf maker (amountOf maker - amountOf trade)], [trade])
        Just trade ->
          ([], [trade])
        Nothing -> 
          ([maker], [])
  in
    foldMap (\maker -> go maker (matchOrders maker taker)) makers
    

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

-- instance Semigroup (Book asset) where
  -- Book bids1 asks1 <> Book bids2 asks2 = 
    -- Book (bids1 <> bids2) (asks1 <> asks2)

-- instance Monoid (Book asset) where
  -- mempty = emptyBook

instance Foldable Book where
  foldr f x0 (Book bids asks) = 
    foldr f x0 (assetOf <$> bids ++ asks)

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
newOrder order book | otherwise = 
  book { asks = insertBy (comparing priceOf) order (asks book) }


-- Exchange
type Exchange asset = 
  State (Book asset) 

runExchangeWith :: Book asset -> Exchange asset a -> a
runExchangeWith book ex = State.evalState ex book

runExchange :: Exchange asset a -> a
runExchange = runExchangeWith emptyBook

printExchange :: Exchange asset ()
printExchange = undefined


emptyExchange :: Exchange asset ()
emptyExchange = 
  return ()


placeOrder :: Order asset -> Exchange asset [Trade asset]
placeOrder order = do
  book <- State.get 
  if isBid order
    then do
      -- let (ts, as) = mapAccumL (needsNewName order) [] (asks book)
      let (as, ts) = tradeOrders (Maker <$> asks book) (Taker order)
      State.put $ book { asks = ((\(Maker order) -> order) <$> as) }
      return ts
    else do
      -- let (ts, bs) = mapAccumL (needsNewName order) [] (bids book)
      let (bs, ts) = tradeOrders (Maker <$> bids book) (Taker order)
      State.put $ book { bids = ((\(Maker order) -> order) <$> bs) }
      return ts

exchangeBook :: Exchange asset (Book asset)
exchangeBook =
  State.get
