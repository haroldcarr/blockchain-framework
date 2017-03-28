{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CommandDispatcher
import           Consensus
import           Http                 (commandReceiver)
import           Ledger               (EData)
import           LedgerImpl           (Ledger, LedgerEntry, addLedgerEntry,
                                       generateNextLedgerEntry, getEntry,
                                       getLastCommittedEntry, isValidLedger,
                                       mkLedger)
import           LedgerImplState      (initialLedgerImplState)
import           Logging              (configureLogging)
import           TransportUDP         (startNodeComm)

import           Control.Concurrent   (MVar, newEmptyMVar, putMVar, takeMVar,
                                       withMVar)
import           Data.Aeson           (encode)
import           Data.ByteString.Lazy (toStrict)
import           Network.Socket       (HostName, PortNumber)
import           System.Environment   (getArgs)

defaultHost :: HostName
defaultHost  = "224.0.0.99"
defaultPort :: PortNumber
defaultPort  = 9160

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    []             -> doIt defaultPort                   defaultHost (read (show defaultPort) :: PortNumber)
    [httpPort,h,p] -> doIt (read httpPort :: PortNumber) h           (read p                  :: PortNumber)
    xss            -> error (show xss)

doIt :: PortNumber -> HostName -> PortNumber -> IO ()
doIt httpPort host port = do
  configureLogging
  commandDispatcher <- initializeCommandDispatcher
  startNodeComm commandDispatcher host port
  commandReceiver commandDispatcher "0.0.0.0" httpPort

initializeCommandDispatcher :: IO CommandDispatcher
initializeCommandDispatcher = do
  ledgerState <- initialLedgerImplState
  mv <- newEmptyMVar
  return (CommandDispatcher
          Consensus.handleConsensusMessage
          (getMsgsToSendToConsensusNodes mv)
          (sendToConsensusNodes mv)
          (Main.listBlocks ledgerState)
          (Main.addBlock ledgerState mv)
          (Main.isValid ledgerState))

getMsgsToSendToConsensusNodes :: MVar EData -> IO EData
getMsgsToSendToConsensusNodes  = takeMVar

sendToConsensusNodes :: MVar EData -> EData -> IO ()
sendToConsensusNodes  = putMVar

listBlocks :: MVar Ledger -> Maybe Int -> IO (Maybe Ledger)
listBlocks ledger i =
  case i of
    -- return all entries
    Nothing -> withMVar ledger $ return . Just
    -- return the single entry (as a one-element list)
    Just i' -> withMVar ledger $ \es -> case getEntry es i' of
                                          Nothing -> return Nothing
                                          Just el -> return (Just (mkLedger el))

addBlock :: MVar Ledger -> MVar EData -> EData -> IO LedgerEntry
addBlock ledger sendToConsensusNodesMV edata =
  withMVar ledger $ \ledger' -> do
    let newLedgerEntry = generateNextLedgerEntry (getLastCommittedEntry ledger') "fake timestamp" edata
    -- send entry to verifiers
    putMVar sendToConsensusNodesMV (toStrict (encode (AppendEntry newLedgerEntry)))
    -- return entry to caller
    return newLedgerEntry

isValid :: MVar Ledger -> LedgerEntry -> IO (Maybe String)
isValid ledger ledgerEntry =
  withMVar ledger $ \l -> return $ isValidLedger (addLedgerEntry ledgerEntry l)

