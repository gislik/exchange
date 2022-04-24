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
  setAmountOf :: a asset -> Amount -> a asset
  setTimeOf :: a asset -> Time -> a asset

-- amount
incAmountOf :: Entry a asset => a asset -> Amount -> a asset
incAmountOf entry amount =
  setAmountOf entry (amountOf entry + amount)

decAmountOf :: Entry a asset => a asset -> Amount -> a asset
decAmountOf entry amount =
  setAmountOf entry (amountOf entry - amount)

-- time
incTimeOf :: Entry a asset => a asset -> Time -> a asset
incTimeOf entry time =
  setTimeOf entry (timeOf entry + time)

decTimeOf :: Entry a asset => a asset -> Time -> a asset
decTimeOf entry time =
  setTimeOf entry (timeOf entry - time)

-- cost
costOf :: GetEntry a asset => a asset -> Cost 
costOf entry =
  (priceOf entry) `times` (amountOf entry)
