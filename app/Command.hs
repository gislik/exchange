module Command where

import qualified Data.Char as Char
import qualified Text.ParserCombinators.ReadP as Read
import qualified Exchange.Order as Order
import Text.ParserCombinators.ReadP (ReadP)
import Exchange.Order (Order)
import Exchange

data Command asset =
    Order (Order.Taker asset)
  | Cancel (Order.Maker asset)
  | Unknown 
  | Exit
  deriving (Show)

instance Read asset => Read (Command asset) where
  readsPrec _ = 
    Read.readP_to_S $
      readCommand

readCommand :: Read asset => ReadP (Command asset)
readCommand = do
  Read.skipSpaces
  str <- map Char.toLower <$> Read.munch1 (Char.isAlphaNum)
  case str of
    "bid" -> 
      Order <$> readTaker Bid
    "ask" -> 
      Order <$> readTaker Ask
    "cancel" -> do
      side <- readP
      Cancel <$> readMaker side
    "exit" -> 
      return Exit
    _ ->
      return Unknown 

readOrder :: Read asset => Side -> ReadP (Order asset)
readOrder side = do
  Read.skipSpaces
  Order.limit 
    <$> pure side 
    <*> readP
    <*> pure mempty 
    <*> (Amount <$> readP) 
    <*> (Price <$> readP)

readTaker :: Read asset => Side -> ReadP (Order.Taker asset)
readTaker side = do
  order <- readOrder side
  return (Order.Taker order)

readMaker :: Read asset => Side -> ReadP (Order.Maker asset)
readMaker side = do
  order <- readOrder side
  return (Order.Maker order)

readP :: Read a => ReadP a
readP = Read.readS_to_P reads
