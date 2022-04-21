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

-- Time
newtype Time = 
  Time Int
    deriving (Show, Eq, Num, Enum)

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


