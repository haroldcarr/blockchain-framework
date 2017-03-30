{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ConsensusImpl        as CI (AppendEntry (..),
                                             ConsensusCommunicationOps (..),
                                             SendToConsensusNodes,
                                             recFromConsensusNodes')

import           Http                 (commandReceiver)
import           Ledger
import           LedgerImpl           (LedgerImpl, generateNextLedgerEntryInfo,
                                       genesisLedger, isValidEntry')
import           Logging              (configureLogging)
import           SystemWiring         as SW
import           TransportUDP         (startNodeComm)

import           Control.Concurrent   (MVar, newEmptyMVar, newMVar, putMVar,
                                       takeMVar, withMVar)
import           Data.Aeson           (ToJSON, encode)
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
  (cd, cc) <- initializeWiring
  startNodeComm host port
                (CI.recFromConsensusNodes cc) (CI.getMsgToSendToConsensusNodes cc) (CI.sendToConsensusNodes cc)
  commandReceiver "0.0.0.0" httpPort (SW.listEntries cd) (SW.addEntry cd)

initializeWiring :: IO (SystemWiring LedgerImpl, ConsensusCommunicationOps)
initializeWiring = do
  ledgerState <- newMVar genesisLedger
  commMV <- newEmptyMVar
  let iv   = Main.isValid ledgerState
      send = (putMVar commMV)
  return ( SystemWiring
            (Main.listEntries ledgerState)
            (Main.addEntry ledgerState send)
         , ConsensusCommunicationOps
            (CI.recFromConsensusNodes' iv)
            (takeMVar commMV) -- getMsgsToSendToConsensusNodes
            send              -- sendToConsensusNodes
         )

listEntries :: (ToJSON l, Ledger l) => MVar l -> Maybe Int -> IO (Maybe l)
listEntries ledger i = withMVar ledger $ \l -> return (Ledger.listEntries l i)

addEntry :: MVar LedgerImpl -> SendToConsensusNodes -> EData -> IO (String, String, String)
addEntry ledger sendToConsensusNodes0 edata0 =
  withMVar ledger $ \ledger' -> do
    let (i, _, ts, _, h) = generateNextLedgerEntryInfo ledger' "fake timestamp" edata0
    -- send entry to verifiers
    sendToConsensusNodes0 (toStrict (encode (AppendEntry "AER" i ts edata0 h)))
    -- return entry to caller
    return (show i, show ts, show h)

isValid :: MVar LedgerImpl -> EIndex -> ETimestamp -> EData -> EHash -> IO (Maybe String)
isValid ledger i t d h = withMVar ledger $ \l -> return (isValidEntry' l i t d h)

