module Command where

import qualified Data.Char as Char
import qualified Text.ParserCombinators.ReadP as Read
import qualified Exchange.Order as Order
import Text.ParserCombinators.ReadP (ReadP)
import System.IO.Error (isEOFError)
import Control.Exception (ErrorCall, Handler(..), handleJust, catches)
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
  | ParseError String
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
      amount <- readAmount
      return $ Deposit amount
    "withdraw" -> do
      amount <- readAmount
      return $ Withdraw amount
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
    <*> readAmount
    <*> readPrice

readTaker :: Read asset => Side -> ReadP (Order.Taker asset)
readTaker side = do
  order <- readOrder side
  return (Order.Taker order)

readMaker :: Read asset => Side -> ReadP (Order.Maker asset)
readMaker side = do
  order <- readOrder side
  return (Order.Maker order)

readAmount :: ReadP Amount
readAmount = do
  amount <- readP 
  if amount < 0
    then errorWithoutStackTrace "amount must be non-negative"
    else return (toAmount amount)

readPrice :: ReadP Price
readPrice = do
  price <- readP 
  if price < 0
    then errorWithoutStackTrace "price must be non-negative"
    else return (toPrice price)

readP :: Read a => ReadP a
readP = 
  Read.readS_to_P reads

newline :: IO ()
newline =
  putStrLn ""

getCommand :: Read asset => IO (Command asset)
getCommand = do
  (handleIOError readLn) `catches` [Handler parseErrorHandler]


parseErrorHandler :: ErrorCall -> IO (Command asset)
parseErrorHandler e = do
  return $ ParseError (show e)

handleIOError :: IO (Command asset) -> IO (Command asset)
handleIOError = do
  let
    selector err = 
      if isEOFError err -- ctrl-d
        then Just Exit
        else Just (ParseError "command parse error") -- parse errors while readLn happen in io
  handleJust selector return


