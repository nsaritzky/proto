module Main where

import GetLogs (loadData, printSummaryData)
import System.Environment (getArgs)

main :: IO ()
main = do
  fileName <- head <$> getArgs
  txns <- loadData fileName
  printSummaryData txns
