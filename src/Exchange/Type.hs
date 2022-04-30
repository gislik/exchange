{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Exchange.Type
  ( Amount,
    Price,
    Cost (..),
    Time (..),
    Side (..),
    Style (..),
    toAmount,
    toPrice,
    times,
    readString,
  )
where

import qualified Data.Char as Char
import Data.Typeable (Typeable, typeOf)
import Text.ParserCombinators.ReadP (ReadP, (+++))
import qualified Text.ParserCombinators.ReadP as Read

-- Amount
newtype Amount asset
  = Amount Double
  deriving (Eq, Ord, Num, Typeable)

toAmount :: Double -> Amount asset
toAmount d =
  if d < 0
    then error "Amount can only be non-negative"
    else Amount d

instance (Typeable asset) => Read (Amount asset) where
  readsPrec _ =
    Read.readP_to_S $ do
      readType toAmount

instance Semigroup (Amount asset) where
  Amount d <> Amount e =
    Amount (d + e)

instance Monoid (Amount asset) where
  mempty =
    Amount 0.0

instance (Show asset, Typeable asset) => Show (Amount asset) where
  showsPrec i amount@(Amount d) =
    let asset =
          head . drop 1 . words . show $ typeOf amount
     in showsPrec i d . showChar ' ' . showString asset

-- Price
newtype Price asset
  = Price Double
  deriving (Eq, Ord, Num, Read)

toPrice :: Double -> Price asset
toPrice d =
  if d < 0
    then error "Price can only be non-negative"
    else Price d

instance Semigroup (Price asset) where
  Price price1 <> Price price2 =
    Price (price1 + price2)

instance Monoid (Price asset) where
  mempty =
    Price 0

instance (Show asset, Typeable asset) => Show (Price asset) where
  showsPrec i price@(Price d) =
    let asset =
          head . drop 1 . words . show $ typeOf price
     in showsPrec i d . showChar ' ' . showString asset

-- Cost
newtype Cost
  = Cost Double
  deriving (Show, Eq, Ord, Num, Read)

instance Semigroup Cost where
  Cost cost1 <> Cost cost2 =
    Cost (cost1 + cost2)

instance Monoid Cost where
  mempty =
    Cost 0

times :: Price b -> Amount a -> Amount b
times price amount =
  let Price p = price
      Amount a = amount
   in Amount (p * a)

-- Time
newtype Time
  = Time Int
  deriving (Show, Eq, Num, Enum)

instance Semigroup Time where
  Time time1 <> Time time2 =
    Time (time1 + time2)

instance Monoid Time where
  mempty = Time 0

-- Style
data Style
  = Limit
  | AllOrNothing
  deriving (Eq)

data Side
  = Bid
  | Ask
  deriving (Show, Eq)

instance Read Side where
  readsPrec _ =
    Read.readP_to_S $
      readString "Bid" Bid +++ readString "Ask" Ask

readString :: String -> asset -> ReadP asset
readString s x = do
  Read.skipSpaces
  str <- Read.munch1 (Char.isAlphaNum)
  if map Char.toUpper str == map Char.toUpper s
    then return x
    else Read.pfail

readType :: (Num a, Read a, Typeable b) => (a -> b) -> ReadP b
readType f = do
  Read.skipSpaces
  str <- Read.munch1 (Char.isAlphaNum)
  d <- Read.readS_to_P reads
  if str == show (typeOf $ f 0)
    then return (f d)
    else Read.pfail
