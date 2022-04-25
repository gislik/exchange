{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Exchange.Trade where 

import Data.Typeable (Typeable, typeOf)
import Exchange.Type
import Exchange.Entry 

-- Trade
data Trade base quote =
  Trade {
      tradeBaseOf :: base 
    , tradeTimeOf :: Time 
    , tradeAmountOf :: Amount base
    , tradePriceOf :: Price
    }
  deriving (Eq)

instance (Show base, Show quote, Typeable base, Typeable quote) => Show (Trade base quote) where
  showsPrec i trade = 
    let
      go =
        showString (head . words . show $ typeOf trade) . showChar ' ' . 
        -- showsPrec i (sideOf trade) . showChar ' ' . 
        showsPrec 11 (timeOf trade) . showChar ' ' .
        showsPrec 11 (amountOf trade) . showChar ' ' .
        showsPrec i (baseOf trade) . showChar ' ' .
        showString "@ " .
        showsPrec 11 (priceOf trade) 
    in
      if i > 0
        then showParen True go
        else go

instance GetEntry Trade base quote where
  -- sideOf = tradeSideOf
  sideOf   = const Bid -- TODO: hardcoded
  baseOf   = tradeBaseOf
  timeOf   = tradeTimeOf
  amountOf = tradeAmountOf
  priceOf  = tradePriceOf

instance SetEntry Trade base quote where
  setAmountOf trade amount = trade { tradeAmountOf = amount }
  setTimeOf trade time     = trade { tradeTimeOf = time }


