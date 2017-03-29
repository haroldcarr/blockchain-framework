{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CommandDispatcher    as CD
import           ConsensusImpl
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
  cd <- initializeCommandDispatcher
  startNodeComm  host port (CD.recFromConsensusNodes cd) (CD.getMsgToSendToConsensusNodes cd)
                           (CD.sendToConsensusNodes cd)  (CD.isValid cd)
  commandReceiver "0.0.0.0" httpPort (CD.listEntries cd) (CD.addEntry cd)

initializeCommandDispatcher :: IO (CommandDispatcher LedgerEntryImpl LedgerImpl)
initializeCommandDispatcher = do
  ledgerState <- newMVar genesisLedger
  mv <- newEmptyMVar
  return (CommandDispatcher
          ConsensusImpl.recFromConsensusNodes
          (takeMVar mv) -- getMsgsToSendToConsensusNodes
          (putMVar mv)  -- sendToConsensusNodes
          (listEntries' ledgerState)
          (Main.addEntry ledgerState mv)
          (Main.isValid ledgerState))

listEntries' :: MVar LedgerImpl -> Maybe Int -> IO (Maybe LedgerImpl)
listEntries' ledger i = withMVar ledger $ \l -> return (Ledger.listEntries l i)

addEntry :: MVar LedgerImpl -> MVar EData -> EData -> IO LedgerEntryImpl
addEntry ledger sendToConsensusNodesMV edata0 =
  withMVar ledger $ \ledger' -> do
    let nle = generateNextLedgerEntry (getLastCommittedEntry ledger') "fake timestamp" edata0
    -- send entry to verifiers
    putMVar sendToConsensusNodesMV (toStrict (encode (AppendEntry "AER" (eindex nle) (etimestamp nle) (edata nle) (ehash nle))))
    -- return entry to caller
    return nle

isValid :: MVar LedgerImpl -> EIndex -> ETimestamp -> EData -> EHash -> IO (Maybe String)
isValid ledger i t d h = withMVar ledger $ \l -> return (isValidEntry' l i t d h)

