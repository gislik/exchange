module Command where

import qualified Data.Char as Char
import qualified Text.ParserCombinators.ReadP as Read
import qualified Exchange.Order as Order
import Text.ParserCombinators.ReadP (ReadP)
import System.IO.Error (isEOFError)
import Control.Exception (ErrorCall, Handler(..), handleJust, catches)
import Exchange.Order (Order)
import Exchange

data Command base quote =
    Book base quote
  | Order (Order.Taker base quote)
  | Cancel (Order.Maker base quote)
  | Blotter base
  | Clock
  | Balance 
  | Deposit (Amount quote)
  | Withdraw (Amount quote)
  | ParseError String
  | Unknown 
  | Exit
  deriving (Show)

instance (Read base, Read quote) => Read (Command base quote) where
  readsPrec _ = 
    Read.readP_to_S $
      readCommand

readCommand :: (Read base, Read quote) => ReadP (Command base quote)
readCommand = do
  Read.skipSpaces
  str <- map Char.toLower <$> Read.munch1 (Char.isAlphaNum)
  case str of
    "book" ->
      Book <$> readP <*> readP
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

readOrder :: (Read base, Read quote)  => Side -> ReadP (Order base quote)
readOrder side = do
  Read.skipSpaces
  Order.limit 
    <$> pure side 
    <*> pure mempty 
    <*> readAmount
    <*> readP
    <*> readPrice
    <*> readP

readTaker :: (Read base, Read quote) => Side -> ReadP (Order.Taker base quote)
readTaker side = do
  order <- readOrder side
  return (Order.Taker order)

readMaker :: (Read base, Read quote) => Side -> ReadP (Order.Maker base quote)
readMaker side = do
  order <- readOrder side
  return (Order.Maker order)

readAmount :: ReadP (Amount asset)
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

getCommand :: (Read base, Read quote) => IO (Command base quote)
getCommand = do
  (handleIOError readLn) `catches` [Handler parseErrorHandler]


parseErrorHandler :: ErrorCall -> IO (Command base quote)
parseErrorHandler e = do
  return $ ParseError (show e)

handleIOError :: IO (Command base quote) -> IO (Command base quote)
handleIOError = do
  let
    selector err = 
      if isEOFError err -- ctrl-d
        then Just Exit
        else Just (ParseError "command parse error") -- parse errors while readLn happen in io
  handleJust selector return


