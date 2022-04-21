{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Exchange.Trade where 

import Data.Typeable (Typeable, typeOf)
import Exchange.Type
import Exchange.Entry 

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

instance GetEntry Trade asset where
  -- sideOf = tradeSideOf
  sideOf   = const Bid -- TODO: hardcoded
  assetOf  = tradeAssetOf
  timeOf   = tradeTimeOf
  amountOf = tradeAmountOf
  priceOf  = tradePriceOf

instance SetEntry Trade asset where
  setAmountOf trade amount = trade { tradeAmountOf = amount }
  setTimeOf trade time     = trade { tradeTimeOf = time }


