{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Exchange.Asset where 

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

-- Assets
data ETH = 
  ETH   
    deriving (Show, Eq)

instance Read ETH where
  readsPrec _ = 
    Read.readP_to_S $
      readString "ETH" ETH

data BTC = 
  BTC 
    deriving (Show, Eq)

instance Read BTC where
  readsPrec _ = 
    Read.readP_to_S $
      readString "BTC" BTC


