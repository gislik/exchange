{-# LANGUAGE MultiParamTypeClasses #-}
module Exchange.Entry where 

import Exchange.Type

-- Entry
class (GetEntry a base quote, SetEntry a base quote) => Entry a base quote

class GetEntry a base quote where
  sideOf   :: a base quote -> Side
  baseOf   :: a base quote -> base
  quoteOf  :: a base quote -> quote
  timeOf   :: a base quote -> Time
  amountOf :: a base quote -> Amount base
  priceOf  :: a base quote -> Price quote

class SetEntry a base quote where
  setAmountOf :: a base quote -> Amount base -> a base quote
  setTimeOf :: a base quote -> Time -> a base quote

-- amount
incAmountOf :: Entry a base quote => a base quote -> Amount base -> a base quote
incAmountOf entry amount =
  setAmountOf entry (amountOf entry + amount)

decAmountOf :: Entry a base quote => a base quote -> Amount base -> a base quote
decAmountOf entry amount =
  setAmountOf entry (amountOf entry - amount)

-- time
incTimeOf :: Entry a base quote => a base quote -> Time -> a base quote
incTimeOf entry time =
  setTimeOf entry (timeOf entry + time)

decTimeOf :: Entry a base quote => a base quote -> Time -> a base quote
decTimeOf entry time =
  setTimeOf entry (timeOf entry - time)

-- cost
costOf :: GetEntry a base quote => a base quote -> Amount quote
costOf entry =
  (priceOf entry) `times` (amountOf entry)
