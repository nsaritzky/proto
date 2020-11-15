{-# LANGUAGE GADTs #-}

----------------------------------------------------------------------------------------

-- |
-- Module       : GetLogs
--
-- This module provides functions to decode a log file according to the custom protocol
-- format specified in the Proto assignment. The loadData function loads such a file
-- into a list of structured transaction records, and the printSummaryData function
-- prints from such a list.
----------------------------------------------------------------------------------------

module GetLogs
  ( loadData,
    printSummaryData,
  )
where

import Control.Monad (guard, liftM3, liftM4)
import Control.Monad.Combinators (some)
import Data.Binary (Binary, decode, get, put)
import Data.Binary.Get
  ( Get,
    getDoublebe,
    getLazyByteString,
    getWord32be,
    getWord64be,
    getWord8,
    label,
    lookAhead,
    runGet,
  )
import Data.Binary.Put (putWord8)
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word32, Word64, Word8)
import System.IO (FilePath)
import Text.Printf (printf)

-- Magical constants
magicStr = "MPS7"

magicAcct = 2456938384156277127

--
data Header = Header
  { magicString :: String,
    version :: Word8,
    records :: Word32
  }
  deriving (Eq, Show)

-- We make a type to capture the four transaction types.
data RecordType
  = Credit
  | Debit
  | StartAutopay
  | StopAutopay
  deriving (Eq, Show)

-- The encoding/decoding for RecordType are implemented by way of an instance for the
-- Binary class. This allows us to use Data.Binary.decode function to capture it for
-- consistency with all of our other getters defined below.
instance Binary RecordType where
  put Debit = putWord8 0
  put Credit = putWord8 1
  put StartAutopay = putWord8 2
  put StopAutopay = putWord8 3

  -- Throws exception on invalid Word8 inputs.
  get = do
    tag <- getWord8
    case tag of
      0 -> return Debit
      1 -> return Credit
      2 -> return StartAutopay
      3 -> return StopAutopay

-- Encodes transaction information. The type of amount is Maybe Double to allow for
-- "Nothing"s in the case of Start/StopAutopay transactions.
data Transaction = Transaction
  { recType :: RecordType,
    timestamp :: UTCTime,
    userID :: Word64,
    amount :: Maybe Double
  }
  deriving (Eq, Show)

----------------------------------------------------------------------------------------

-- The following functions are all getters for the elementary fields in the logfile
-- specification. They are all written using the Data.Binary.Get monad, and they all
-- work similarly.

-- This getter is written to fail if the first four bytes don't match the magic
-- string.
getMagic :: Get String
getMagic = do
  x <- LC.unpack <$> getLazyByteString 4
  guard (x == magicStr)
  return x

getVersion :: Get Word8
getVersion = label "Version" getWord8

getCount :: Get Word32
getCount = label "Record Count" getWord32be

getType :: Get RecordType
getType = decode <$> getLazyByteString 1

-- This function is needed for its Word32 type declaration to avoid ambiguity in the use
-- of "decode" in the getTime function.
decodeTime :: Word32 -> UTCTime
decodeTime = posixSecondsToUTCTime . realToFrac

getTime :: Get UTCTime
getTime = decodeTime . decode <$> getLazyByteString 4

getUserID :: Get Word64
getUserID = label "User ID" getWord64be

-- The "Nothing" case is handled in getTransaction.
getAmt :: Get (Maybe Double)
getAmt = Just <$> getDoublebe

------------------------------------------------------------

-- The following two getters compose the above by lifting the respective Data
-- constructors to the Get monad.

getHeader :: Get Header
getHeader = label "Header" $ liftM3 Header getMagic getVersion getCount

getTransaction :: Get Transaction
getTransaction = label "Transaction" $ do
  x <- lookAhead getType -- Verify the magic string without consuming input Partial
  -- application of the lifted Transaction cunstructor, with the last field filled in by
  -- case.
  let f = liftM4 Transaction getType getTime getUserID
  if x `elem` [Debit, Credit]
    then f getAmt
    else f (pure Nothing)

----------------------------------------------------------------------------------------

-- The next block of functions filter and accumulate the captured transaction data to
-- what is called for in the specification

-- Filter a list of transactions by RecordType
filterTxns :: RecordType -> [Transaction] -> [Transaction]
filterTxns rType = filter ((== rType) . recType)

-- Add up the amounts of all Transactions in a list of a particular RecordType. The
-- usage of fromJust means that this throws an exception if rType is Start/StopAutopay,
-- which both result in Nothing for the amount field.
txnSum :: RecordType -> [Transaction] -> Double
txnSum rType txns =
  let filteredTxns = filterTxns rType txns
   in sum (fromJust . amount <$> filteredTxns)

creditSum :: [Transaction] -> Double
creditSum = txnSum Credit

debitSum :: [Transaction] -> Double
debitSum = txnSum Debit

-- Count the Start/StopAutopay transactions. These will count multiples for each userID
-- even if it might not make sense. E.g. two StartAutopays in a row.
autopayStarts :: [Transaction] -> Int
autopayStarts = length . filterTxns StartAutopay

autopayStops :: [Transaction] -> Int
autopayStops = length . filterTxns StopAutopay

-- Compute the account balance for the magic userID based on list of Transactions.
acctBalance :: [Transaction] -> Double
acctBalance txns =
  let acctTxns = filter ((== magicAcct) . userID) txns
   in txnSum Credit acctTxns - txnSum Debit acctTxns

----------------------------------------------------------------------------------------

-- Finally, the IO functions for reading the file and printing the summary.

-- Loads a list of Transactions from a file encoded according to the MPS7
-- specification. Throws an exception on invalid encoding.
loadData :: FilePath -> IO [Transaction]
loadData fileName = do
  contents <- LC.readFile fileName
  return $ runGet (getHeader >> some getTransaction) contents

-- Prints the specified summary data from a list of transactions.
printSummaryData :: [Transaction] -> IO ()
printSummaryData txns =
  do
    printf "total credit amount=%.2f\n" (creditSum txns)
    printf "total debit amount=%.2f\n" (debitSum txns)
    printf "autopays started=%v\n" (autopayStarts txns)
    printf "autopays stopped=%v\n" (autopayStops txns)
    printf "balance for user %v=%.2f\n" magicAcct (acctBalance txns)
