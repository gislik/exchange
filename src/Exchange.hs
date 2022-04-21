{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Exchange where 

import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Text.ParserCombinators.ReadP as Read
import GHC.Read (Read, readPrec)
import Data.Typeable (Typeable, typeOf)
import Data.List (insertBy, groupBy)
import Data.Function (on)
import Data.Ord (Down(Down), comparing)
import Text.ParserCombinators.ReadP (ReadP, (+++))
import Control.Monad.Trans.State.Strict (StateT)
import Data.Monoid (Ap(..))

-- Assets
data ETH = 
  ETH   
    deriving (Show, Eq)

instance Read ETH where
  readsPrec _ = 
    Read.readP_to_S $
      readString "ETH" ETH

data BTC = 
  BTC 
    deriving (Show, Eq)

instance Read BTC where
  readsPrec _ = 
    Read.readP_to_S $
      readString "BTC" BTC

readString :: String -> asset -> ReadP asset
readString s x = do
    Read.skipSpaces
    str <- Read.munch1 (Char.isAlphaNum)
    if map Char.toUpper str == map Char.toUpper s
      then return x
      else Read.pfail

-- Amount
newtype Amount = 
  Amount Double 
    deriving (Show, Eq, Ord, Num, Read)

instance Semigroup Amount where
  Amount d <> Amount e = 
    Amount (d + e)
  
instance Monoid Amount where
  mempty = 
    Amount 0.0

-- Price
newtype Price = 
  Price Double 
    deriving (Show, Eq, Ord, Num, Read)

-- Time
newtype Time = 
  Time Int
    deriving (Show, Eq)

data Side =
    Bid 
  | Ask
  deriving (Show, Eq)

instance Read Side where
  readsPrec _ = 
    Read.readP_to_S $
      readString "Bid" Bid +++ readString "Ask" Ask

-- Entry
class Entry a asset where
  sideOf   :: a asset -> Side
  assetOf  :: a asset -> asset
  timeOf   :: a asset -> Time
  amountOf :: a asset -> Amount
  priceOf  :: a asset -> Price

  setAmountOf :: a asset -> Amount -> a asset

incAmountOf :: Entry a asset => a asset -> Amount -> a asset
incAmountOf entry amount =
  setAmountOf entry (amountOf entry + amount)

decAmountOf :: Entry a asset => a asset -> Amount -> a asset
decAmountOf entry amount =
  setAmountOf entry (amountOf entry - amount)

-- Trade
data Trade asset =
  Trade {
      tradeAssetOf :: asset 
    , tradeTimeOf :: Time 
    , tradeAmountOf :: Amount 
    , tradePriceOf :: Price
    }
  deriving (Eq)

instance (Show asset, Typeable asset) => Show (Trade asset) where
  showsPrec i trade = 
    let
      go =
        showString (head . words . show $ typeOf trade) . showChar ' ' . 
        -- showsPrec i (sideOf trade) . showChar ' ' . 
        showsPrec i (assetOf trade) . showChar ' ' .
        showsPrec 11 (timeOf trade) . showChar ' ' .
        showsPrec 11 (amountOf trade) . showChar ' ' .
        showsPrec 11 (priceOf trade) 
    in
      if i > 0
        then showParen True go
        else go

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
  deriving (Eq, Typeable)

instance (Show asset, Typeable asset) => Show (Order asset) where
  showsPrec i order = 
    let
      go =
        showString (head . words . show $ typeOf order) . showChar ' ' . 
        showsPrec i (sideOf order) . showChar ' ' . 
        showsPrec i (assetOf order) . showChar ' ' .
        showsPrec 11 (timeOf order) . showChar ' ' .
        showsPrec 11 (amountOf order) . showChar ' ' .
        showsPrec 11 (priceOf order) 
    in
      if i > 0
        then showParen True go
        else go

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
    amount = min (amountOf maker) (amountOf taker)
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
    decAmountBy maker trade = 
      decAmountOf maker (amountOf trade)
    remainingAmount maker trade =
      amountOf maker - amountOf trade 
    go maker (makers', trades', amount') trade' =
      case trade' of 
        Just trade | remainingAmount maker trade > 0 -> 
          (decAmountBy maker trade:makers', trade:trades', amountOf trade + amount')
        Just trade ->
          (makers', trade:trades', amount' + amountOf trade)
        Nothing -> 
          (maker:makers', trades', amount')
    f maker state@(_, _, amt') = 
      go maker state (matchOrders maker (decAmountOf taker amt'))
    (ms, ts, _) = 
      foldr f ([],[], (Amount 0)) (reverse makers)
  in
    (reverse ms, reverse ts)
    
printOrder :: Typeable asset => Maker asset -> IO ()
printOrder order = do
  putStr $ show (sideOf order)
  putStr $ " (" ++ show (priceOf order) ++ ")"
  putStr $ " (" ++ show (amountOf order) ++ ")"
  putStrLn ""

sumOrderAmount :: List.NonEmpty (Maker asset) -> Maker asset
sumOrderAmount orders = 
  let
    order  = NonEmpty.head orders
    amount = foldMap amountOf orders
  in
    setAmountOf order amount

groupOrdersBy :: Eq b => (Maker asset -> b) -> [Maker asset] -> [List.NonEmpty (Maker asset)]
groupOrdersBy f orders = 
  NonEmpty.fromList <$> groupBy (equalOn f) orders

equalOn :: Eq b => (a -> b) -> a -> a -> Bool
equalOn f = (==) `on` f

-- Book
data Book asset = Book {
    bids :: [Maker asset]
  , asks :: [Maker asset]
} deriving (Show, Typeable)

instance Foldable Book where
  foldr f x0 book = 
    foldr f x0 (assetOf <$> bids book ++ asks book)

printBook :: (Show asset, Typeable asset) => Book asset -> IO ()
printBook book = do
  let typeOfBook = show (typeOf book)
  putStrLn typeOfBook
  putStrLn (replicate (length typeOfBook) '=')
  mapM_ printOrder $ 
    sumOrderAmount <$> groupOrdersBy priceOf (reverse $ asks book)
  putStrLn ""
  mapM_ printOrder $ 
    sumOrderAmount <$> groupOrdersBy priceOf (bids book)
  putStrLn ""

emptyBook :: Book asset
emptyBook = 
  Book [] []

newOrder :: Maker asset -> Book asset -> Book asset
newOrder order book | isBid order = 
  book { bids = insertBy (comparing (Down . priceOf)) order (bids book) }
newOrder order book | otherwise = 
  book { asks = insertBy (comparing priceOf) order (asks book) }


-- Exchange
type Exchange asset m = 
  StateT (Book asset) m 

runWith :: Monad m => Book asset -> Exchange asset m a -> m a
runWith book ex = 
  State.evalStateT ex book

run :: Monad m => Exchange asset m a -> m a
run = 
  runWith emptyBook

empty :: Monad m => Exchange asset m ()
empty = 
  return ()


place :: Monad m => Order asset -> Exchange asset m [Trade asset]
place order = do
  book <- State.get 
  if isBid order
    then do
      let (as, ts) = tradeOrders (asks book) (Taker order)
      State.put $ book { asks = as }
      return ts
    else do
      let (bs, ts) = tradeOrders (bids book) (Taker order)
      State.put $ book { bids = bs }
      return ts

orderbook :: Monad m => Exchange asset m (Book asset)
orderbook =
  State.get
