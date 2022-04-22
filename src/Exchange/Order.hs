{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Exchange.Order where 

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty as List (NonEmpty)
import Data.Typeable (Typeable, typeOf)
import Data.Function (on)
import Data.Foldable (foldl')
import Control.Arrow ((***))
import Exchange.Trade (Trade(Trade))
import Exchange.Entry 
import Exchange.Type

-- Order
data Order asset = 
  Order {
      orderSideOf :: Side
    , orderAssetOf :: asset
    , orderTimeOf :: Time
    , orderAmountOf :: Amount
    , orderPriceOf :: Price
    , orderStyleOf :: Style
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

instance GetEntry Order asset where
  sideOf   = orderSideOf
  assetOf  = orderAssetOf
  timeOf   = orderTimeOf
  amountOf = orderAmountOf
  priceOf  = orderPriceOf

instance SetEntry Order asset where
  setAmountOf order amount = order { orderAmountOf = amount }
  setTimeOf order time     = order { orderTimeOf = time }

isBid :: GetEntry a asset => a asset -> Bool
isBid order =
  case sideOf order of
    Bid -> True
    _   -> False

isAsk :: GetEntry a asset => a asset -> Bool
isAsk order =
  case sideOf order of
    Ask -> True
    _   -> False

limit :: Side -> asset -> Time -> Amount -> Price -> Order asset
limit side asset time amount price =
  Order {
    orderSideOf   = side
  , orderAssetOf  = asset
  , orderTimeOf   = time
  , orderAmountOf = amount
  , orderPriceOf  = price
  , orderStyleOf  = Limit
  }

fillAndKill :: Side -> asset -> Time -> Amount -> Price -> Order asset
fillAndKill side asset time amount price =
  let 
    order = 
      limit side asset time amount price 
    in
      order { orderStyleOf = FillAndKill }

-- Maker
newtype Maker asset = 
  Maker (Order asset)
    deriving (Show, Eq)

instance GetEntry Maker asset where
  sideOf   (Maker order) = sideOf order
  assetOf  (Maker order) = assetOf order
  timeOf   (Maker order) = timeOf order
  amountOf (Maker order) = amountOf order
  priceOf  (Maker order) = priceOf order

instance SetEntry Maker asset where
  setAmountOf (Maker order) amount = Maker order { orderAmountOf = amount }
  setTimeOf (Maker order) time     = Maker order { orderTimeOf = time }

instance Entry Maker asset

sumAmount :: List.NonEmpty (Maker asset) -> Maker asset
sumAmount orders = 
  let
    order  = NonEmpty.head orders
    amount = foldMap amountOf orders
  in
    setAmountOf order amount

equalOn :: Eq b => (a -> b) -> a -> a -> Bool
equalOn f = (==) `on` f

groupBy :: Eq b => (Maker asset -> b) -> [Maker asset] -> [List.NonEmpty (Maker asset)]
groupBy f orders = 
  NonEmpty.fromList <$> List.groupBy (equalOn f) orders

print :: Typeable asset => Maker asset -> IO ()
print order = do
  putStr $ show (sideOf order)
  putStr $ " (" ++ show (priceOf order) ++ ")"
  putStr $ " (" ++ show (amountOf order) ++ ")"
  putStrLn ""

splitSides :: [Maker asset] -> ([Maker asset], [Maker asset])
splitSides makers = 
  List.partition isBid makers

-- Taker
newtype Taker asset = 
  Taker {
    getTakerOrder :: Order asset
  }

instance GetEntry Taker asset where
  sideOf   (Taker order) = sideOf order
  assetOf  (Taker order) = assetOf order
  timeOf   (Taker order) = timeOf order
  amountOf (Taker order) = amountOf order
  priceOf  (Taker order) = priceOf order

instance SetEntry Taker asset where
  setAmountOf (Taker order) amount = Taker order { orderAmountOf = amount }
  setTimeOf (Taker order) time     = Taker order { orderTimeOf = time }

instance Entry Taker asset where

-- Makers and Takers

type EngineState asset = 
  ([Maker asset], [Trade asset], Amount)

accAmount :: EngineState asset -> Amount
accAmount state = 
  case state of
    (_, _, amount) -> amount
    

toMaker :: Taker asset -> Maker asset
toMaker taker = Maker (getTakerOrder taker)

match :: Maker asset -> Taker asset -> Maybe (Trade asset)
match maker taker = 
  let
    asset  = assetOf taker
    time   = timeOf taker
    amount = min (amountOf maker) (amountOf taker)
    price  = priceOf maker
    trade'  = Trade asset time amount price
  in
    if isBid taker && isAsk maker && priceOf taker >= priceOf maker && amount > 0
      then Just trade'
    else if isAsk taker && isBid maker && priceOf taker <= priceOf maker && amount > 0
      then Just trade'
    else 
      Nothing

trade :: Taker asset -> [Maker asset] -> ([Maker asset], [Trade asset])
trade taker makers = 
  let
    order = 
      getTakerOrder taker
    f state maker = 
      engine_ (match maker (decAmountOf taker (accAmount state))) maker state 
    (makers', trades', amount') = 
      foldl' f mempty makers
  in
   reverse *** reverse $
      case orderStyleOf order of
        Limit | (not . null) trades' && amountOf taker > amount' ->
          (toMaker (decAmountOf taker amount'):makers', trades')
        Limit | amountOf taker > amount' ->
          (toMaker taker:makers', trades')
        _ ->
          (makers', trades')
    
engine_ :: Maybe (Trade asset) -> Maker asset -> EngineState asset -> EngineState asset
engine_ mtrade maker (makers', trades', amount') =
  let
    decAmountBy maker' trade' = 
      decAmountOf maker' (amountOf trade')
    accAmountOf trade' =
      amount' + amountOf trade'
    isMakerAfter trade' =
      amountOf maker - amountOf trade' > 0
  in
    case mtrade of 
      Just trade' | isMakerAfter trade' -> 
        (decAmountBy maker trade':makers', trade':trades', accAmountOf trade')
      Just trade' ->
        (makers', trade':trades', accAmountOf trade')
      Nothing | otherwise -> 
        (maker:makers', trades', amount')

