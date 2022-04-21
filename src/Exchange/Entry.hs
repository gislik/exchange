{-# LANGUAGE MultiParamTypeClasses #-}

module Exchange.Entry where 

import Exchange.Type

-- Entry
class (GetEntry a asset, SetEntry a asset) => Entry a asset

class GetEntry a asset where
  sideOf   :: a asset -> Side
  assetOf  :: a asset -> asset
  timeOf   :: a asset -> Time
  amountOf :: a asset -> Amount
  priceOf  :: a asset -> Price

class SetEntry a asset where
  setAmountOf :: Amount -> a asset -> a asset
  setTimeOf   :: Time -> a asset -> a asset

-- amount
incAmountOf :: Entry a asset => Amount -> a asset -> a asset
incAmountOf amount entry =
  setAmountOf (amountOf entry + amount) entry

decAmountOf :: Entry a asset => Amount -> a asset -> a asset
decAmountOf amount entry =
  setAmountOf (amountOf entry - amount) entry 


-- time
incTimeOf :: Entry a asset => Time -> a asset -> a asset
incTimeOf time entry =
  setTimeOf (timeOf entry + time) entry 

decTime :: Entry a asset => Time -> a asset -> a asset
decTime time entry =
  setTimeOf (timeOf entry - time) entry 
