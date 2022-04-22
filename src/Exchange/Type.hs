{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Exchange.Type where 

import qualified Data.Char as Char
import qualified Text.ParserCombinators.ReadP as Read
import Text.ParserCombinators.ReadP (ReadP, (+++))

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

instance Semigroup Price where
  Price price1 <> Price price2 =
    Price (price1 + price2)

instance Monoid Price where
  mempty = Price 0

-- Time
newtype Time = 
  Time Int
    deriving (Show, Eq, Num, Enum)

instance Semigroup Time where
  Time time1 <> Time time2 =
    Time (time1 + time2)

instance Monoid Time where
  mempty = Time 0
-- Style
data Style =
    Limit
  | FillAndKill
    deriving (Eq)

data Side =
    Bid 
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


