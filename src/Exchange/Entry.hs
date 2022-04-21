{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Exchange.Entry where 

import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Text.ParserCombinators.ReadP as Read
import GHC.Read (Read, readPrec)
import Data.Typeable (Typeable, typeOf)
import Data.List (insertBy, groupBy)
import Data.Function (on)
import Data.Ord (Down(Down), comparing)
import Text.ParserCombinators.ReadP (ReadP, (+++))
import Control.Monad.Trans.State.Strict (StateT)
import Data.Monoid (Ap(..))
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
