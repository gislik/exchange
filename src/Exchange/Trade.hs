{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Exchange.Trade where

import Data.Typeable (Typeable, typeOf)
import Exchange.Entry
import Exchange.Type

-- Trade
data Trade a b = Trade
  { tradeBaseOf :: a
  , tradeQuoteOf :: b
  , tradeTimeOf :: Time
  , tradeAmountOf :: Amount a
  , tradePriceOf :: Price b
  }
  deriving (Eq)

new :: Time -> Amount a -> a -> Price b -> b -> Trade a b
new time amount base price quote =
  Trade base quote time amount price

instance (Show a, Show b, Typeable a, Typeable b) => Show (Trade a b) where
  showsPrec i trade =
    let go =
          showString (head . words . show $ typeOf trade) . showChar ' '
            -- . showsPrec i (sideOf trade) . showChar ' '
            . showsPrec 11 (timeOf trade)
            . showChar ' '
            . showsPrec 11 (amountOf trade)
            . showChar ' '
            . showString "@ "
            -- . showsPrec i (baseOf trade) . showChar ' '
            . showsPrec 11 (priceOf trade)
     in if i > 0
          then showParen True go
          else go

instance GetEntry Trade a b where
  -- sideOf = tradeSideOf
  sideOf = const Bid -- TODO: hardcoded
  baseOf = tradeBaseOf
  quoteOf = tradeQuoteOf
  timeOf = tradeTimeOf
  amountOf = tradeAmountOf
  priceOf = tradePriceOf

instance SetEntry Trade a b where
  setAmountOf trade amount = trade {tradeAmountOf = amount}
  setTimeOf trade time = trade {tradeTimeOf = time}
