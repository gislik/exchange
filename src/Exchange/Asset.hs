module Exchange.Asset where 

import qualified Text.ParserCombinators.ReadP as Read
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


