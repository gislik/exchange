module Command where

import qualified Data.Char as Char
import qualified Text.ParserCombinators.ReadP as Read
import qualified Exchange.Order as Order
import Text.ParserCombinators.ReadP (ReadP)
import System.IO.Error (isEOFError)
import Control.Exception (ErrorCall, Handler(..), handleJust, catches)
import Exchange.Order (Order)
import Exchange

data Command a b =
    Book a b
  | Order (Order.Taker a b)
  | Cancel (Order.Maker a b)
  | Blotter a
  | Clock
  | Balance 
  | Deposit (Amount b)
  | Withdraw (Amount b)
  | ParseError String
  | Unknown 
  | Exit
  deriving (Show)

instance (Read a, Read b) => Read (Command a b) where
  readsPrec _ = 
    Read.readP_to_S $
      readCommand

readCommand :: (Read a, Read b) => ReadP (Command a b)
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

readOrder :: (Read a, Read b)  => Side -> ReadP (Order a b)
readOrder side = do
  Read.skipSpaces
  Order.limit 
    <$> pure side 
    <*> pure mempty 
    <*> readAmount
    <*> readP
    <*> readPrice
    <*> readP

readTaker :: (Read a, Read b) => Side -> ReadP (Order.Taker a b)
readTaker side = do
  order <- readOrder side
  return (Order.Taker order)

readMaker :: (Read a, Read b) => Side -> ReadP (Order.Maker a b)
readMaker side = do
  order <- readOrder side
  return (Order.Maker order)

readAmount :: ReadP (Amount asset)
readAmount = do
  amount <- readP 
  if amount < 0
    then errorWithoutStackTrace "amount must be non-negative"
    else return (toAmount amount)

readPrice :: ReadP (Price asset)
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

getCommand :: (Read a, Read b) => IO (Command a b)
getCommand = do
  (handleIOError readLn) `catches` [Handler parseErrorHandler]


parseErrorHandler :: ErrorCall -> IO (Command a b)
parseErrorHandler e = do
  return $ ParseError (show e)

handleIOError :: IO (Command a b) -> IO (Command a b)
handleIOError = do
  let
    selector err = 
      if isEOFError err -- ctrl-d
        then Just Exit
        else Just (ParseError "command parse error") -- parse errors while readLn happen in io
  handleJust selector return


