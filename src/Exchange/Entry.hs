{-# LANGUAGE MultiParamTypeClasses #-}
module Exchange.Entry where 

import Exchange.Type

-- Entry
class (GetEntry f a b, SetEntry f a b) => Entry f a b

class GetEntry f a b where
  sideOf   :: f a b -> Side
  baseOf   :: f a b -> a
  quoteOf  :: f a b -> b
  timeOf   :: f a b -> Time
  amountOf :: f a b -> Amount a
  priceOf  :: f a b -> Price b

class SetEntry f a b where
  setAmountOf :: f a b -> Amount a -> f a b
  setTimeOf :: f a b -> Time -> f a b

-- amount
incAmountOf :: Entry f a b => f a b -> Amount a -> f a b
incAmountOf entry amount =
  setAmountOf entry (amountOf entry + amount)

decAmountOf :: Entry f a b => f a b -> Amount a -> f a b
decAmountOf entry amount =
  setAmountOf entry (amountOf entry - amount)

-- time
incTimeOf :: Entry f a b => f a b -> Time -> f a b
incTimeOf entry time =
  setTimeOf entry (timeOf entry + time)

decTimeOf :: Entry f a b => f a b -> Time -> f a b
decTimeOf entry time =
  setTimeOf entry (timeOf entry - time)

-- cost
costOf :: GetEntry f a b => f a b -> Amount b
costOf entry =
  (priceOf entry) `times` (amountOf entry)
