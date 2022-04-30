{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Exchange.Order where

import Control.Arrow ((***))
import Data.Foldable (foldl')
import Data.Function (on)
import qualified Data.List as List
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Typeable (Typeable, typeOf)
import Exchange.Entry
import Exchange.Trade (Trade (Trade))
import Exchange.Type

-- Order
data Order a b = Order
  { orderBaseOf :: a
  , orderQuoteOf :: b
  , orderSideOf :: Side
  , orderTimeOf :: Time
  , orderAmountOf :: Amount a
  , orderPriceOf :: Price b
  , orderStyleOf :: Style
  }
  deriving (Eq, Typeable)

instance (Show a, Show b, Typeable a, Typeable b) => Show (Order a b) where
  showsPrec i order =
    let go =
          showString (head . words . show $ typeOf order) . showChar ' '
            . showsPrec i (sideOf order)
            . showChar ' '
            . showsPrec i (baseOf order)
            . showChar ' '
            . showsPrec 11 (timeOf order)
            . showChar ' '
            . showsPrec 11 (amountOf order)
            . showChar ' '
            . showsPrec 11 (priceOf order)
     in if i > 0
          then showParen True go
          else go

instance GetEntry Order a b where
  sideOf = orderSideOf
  baseOf = orderBaseOf
  quoteOf = orderQuoteOf
  timeOf = orderTimeOf
  amountOf = orderAmountOf
  priceOf = orderPriceOf

instance SetEntry Order a b where
  setAmountOf order amount = order {orderAmountOf = amount}
  setTimeOf order time = order {orderTimeOf = time}

isBid :: GetEntry f a b => f a b -> Bool
isBid order =
  case sideOf order of
    Bid -> True
    _ -> False

isAsk :: GetEntry f a b => f a b -> Bool
isAsk order =
  case sideOf order of
    Ask -> True
    _ -> False

limit :: Side -> Time -> Amount a -> a -> Price b -> b -> Order a b
limit side time amount base price quote =
  Order
    { orderSideOf = side
    , orderBaseOf = base
    , orderQuoteOf = quote
    , orderTimeOf = time
    , orderAmountOf = amount
    , orderPriceOf = price
    , orderStyleOf = Limit
    }

allOrNothing :: Side -> Time -> Amount a -> a -> Price b -> b -> Order a b
allOrNothing side time amount base price quote =
  let order =
        limit side time amount base price quote
   in order {orderStyleOf = AllOrNothing}

-- Maker
newtype Maker a b
  = Maker (Order a b)
  deriving (Show)

instance (Eq a, Eq b) => Eq (Maker a b) where
  Maker order1 == Maker order2 =
    setTimeOf order1 0 == setTimeOf order2 0 -- makers are equal up to their time

instance GetEntry Maker a b where
  sideOf (Maker order) = sideOf order
  baseOf (Maker order) = baseOf order
  quoteOf (Maker order) = quoteOf order
  timeOf (Maker order) = timeOf order
  amountOf (Maker order) = amountOf order
  priceOf (Maker order) = priceOf order

instance SetEntry Maker a b where
  setAmountOf (Maker order) amount = Maker order {orderAmountOf = amount}
  setTimeOf (Maker order) time = Maker order {orderTimeOf = time}

instance Entry Maker a b

sumAmount :: List.NonEmpty (Maker a b) -> Maker a b
sumAmount orders =
  let order = NonEmpty.head orders
      amount = foldMap amountOf orders
   in setAmountOf order amount

equalOn :: Eq b => (a -> b) -> a -> a -> Bool
equalOn f = (==) `on` f

groupBy :: Eq c => (Maker a b -> c) -> [Maker a b] -> [List.NonEmpty (Maker a b)]
groupBy f orders =
  NonEmpty.fromList <$> List.groupBy (equalOn f) orders

print :: (Typeable a, Typeable b, Show a, Show b) => Maker a b -> IO ()
print order = do
  putStr $ show (sideOf order)
  putStr $ " (" ++ show (priceOf order) ++ ")"
  putStr $ " (" ++ show (amountOf order) ++ ")"
  putStrLn ""

splitSides :: [Maker a b] -> ([Maker a b], [Maker a b])
splitSides makers =
  List.partition isBid makers

-- Taker
newtype Taker a b = Taker
  { getTakerOrder :: Order a b
  }

instance GetEntry Taker a b where
  sideOf (Taker order) = sideOf order
  baseOf (Taker order) = baseOf order
  quoteOf (Taker order) = quoteOf order
  timeOf (Taker order) = timeOf order
  amountOf (Taker order) = amountOf order
  priceOf (Taker order) = priceOf order

instance SetEntry Taker a b where
  setAmountOf (Taker order) amount = Taker order {orderAmountOf = amount}
  setTimeOf (Taker order) time = Taker order {orderTimeOf = time}

instance Entry Taker a b

instance (Show a, Show b, Typeable a, Typeable b) => Show (Taker a b) where
  showsPrec i taker@(Taker order) =
    let go =
          showString (head . words . show $ typeOf taker) . showChar ' '
            . showsPrec i order
     in if i > 0
          then showParen True go
          else go

-- Makers and Takers

type EngineState a b =
  ([Maker a b], [Trade a b], Amount a)

accAmount :: EngineState a b -> Amount a
accAmount state =
  case state of
    (_, _, amount) -> amount

toMaker :: Taker a b -> Maker a b
toMaker taker =
  Maker (getTakerOrder taker)

engine_ :: Maybe (Trade a b) -> Maker a b -> EngineState a b -> EngineState a b
engine_ mtrade maker (makers', trades', amount') =
  let decAmountBy maker' trade' =
        decAmountOf maker' (amountOf trade')
      accAmountOf trade' =
        amount' + amountOf trade'
      isMakerAfter trade' =
        amountOf maker - amountOf trade' > 0
   in case mtrade of
        Just trade'
          | isMakerAfter trade' ->
            (decAmountBy maker trade' : makers', trade' : trades', accAmountOf trade')
        Just trade' ->
          (makers', trade' : trades', accAmountOf trade')
        Nothing
          | otherwise ->
            (maker : makers', trades', amount')

match :: Maker a b -> Taker a b -> Maybe (Trade a b)
match maker taker =
  let asset = baseOf taker
      quote = quoteOf taker
      time = timeOf taker
      amount = min (amountOf maker) (amountOf taker)
      price = priceOf maker
      trade' = Trade asset quote time amount price
   in if isBid taker && isAsk maker && priceOf taker >= priceOf maker && amount > 0
        then Just trade'
        else
          if isAsk taker && isBid maker && priceOf taker <= priceOf maker && amount > 0
            then Just trade'
            else Nothing

trade :: Taker a b -> [Maker a b] -> ([Maker a b], [Trade a b])
trade taker makers =
  let order =
        getTakerOrder taker
      f state maker =
        engine_ (match maker (decAmountOf taker (accAmount state))) maker state
      (makers', trades', amount') =
        foldl' f mempty makers
   in reverse *** reverse $
        case orderStyleOf order of
          Limit
            | (not . null) trades' && amountOf taker > amount' ->
              (toMaker (decAmountOf taker amount') : makers', trades')
          Limit
            | amountOf taker > amount' ->
              (toMaker taker : makers', trades')
          _ ->
            (makers', trades')

remove :: (Eq a, Eq b) => Maker a b -> [Maker a b] -> [Maker a b]
remove maker makers =
  filter (/= maker) makers
