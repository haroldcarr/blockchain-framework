{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CommandDispatcher
import           Consensus
import           Http                 (commandReceiver)
import           Ledger
import           LedgerImpl
import           Logging              (configureLogging)
import           TransportUDP         (startNodeComm)

import           Control.Concurrent   (MVar, newEmptyMVar, newMVar, putMVar,
                                       takeMVar, withMVar)
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

initializeCommandDispatcher :: IO (CommandDispatcher LedgerEntryImpl LedgerImpl)
initializeCommandDispatcher = do
  ledgerState <- newMVar genesisLedger
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

listBlocks :: MVar LedgerImpl -> Maybe Int -> IO (Maybe LedgerImpl)
listBlocks ledger i = withMVar ledger $ \l -> return (listEntries l i)

addBlock :: MVar LedgerImpl -> MVar EData -> EData -> IO LedgerEntryImpl
addBlock ledger sendToConsensusNodesMV edata0 =
  withMVar ledger $ \ledger' -> do
    let nle = generateNextLedgerEntry (getLastCommittedEntry ledger') "fake timestamp" edata0
    -- send entry to verifiers
    putMVar sendToConsensusNodesMV (toStrict (encode (AppendEntry "AER" (eindex nle) (etimestamp nle) (edata nle) (ehash nle))))
    -- return entry to caller
    return nle

isValid :: MVar LedgerImpl -> EIndex -> ETimestamp -> EData -> EHash -> IO (Maybe String)
isValid ledger i t d h = withMVar ledger $ \l -> return (isValidEntry' l i t d h)

