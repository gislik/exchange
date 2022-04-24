module Command where

import qualified Data.Char as Char
import qualified Text.ParserCombinators.ReadP as Read
import qualified Exchange.Order as Order
import Text.ParserCombinators.ReadP (ReadP)
import Control.Exception (SomeException, catch)
import Exchange.Order (Order)
import Exchange

data Command asset =
    Book asset
  | Order (Order.Taker asset)
  | Cancel (Order.Maker asset)
  | Blotter asset
  | Clock
  | Balance
  | Deposit Amount
  | Withdraw Amount
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
    "book" ->
      Book <$> readP 
    "bid" -> 
      Order <$> readTaker Bid
    "ask" -> 
      Order <$> readTaker Ask
    "cancel" -> do
      side <- readP
      Cancel <$> readMaker side
    "blotter" -> 
      Blotter <$> readP
    "clock" ->
      return Clock
    "balance" ->
      return Balance
    "deposit" -> do
      amount <- readP
      return $ Deposit (Amount amount)
    "withdraw" -> do
      amount <- readP
      return $ Withdraw (Amount amount)
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

newline :: IO ()
newline =
  putStrLn ""

getCommand :: Read asset => IO (Command asset)
getCommand = do
  readLn `catch` parseErrorHandler 


parseErrorHandler :: SomeException -> IO (Command asset)
parseErrorHandler _ =  do
  return Unknown 

