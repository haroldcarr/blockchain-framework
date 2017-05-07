{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- TODO addEntry - make generic
import           ConsensusImpl      as CI (ConsensusCommunicationWiring (..),
                                           SendToConsensusNodes, addEntry,
                                           recFromConsensusNodes')
import           Http               (commandReceiver)
import           Ledger
import           LedgerImpl         (LedgerImpl, genesisLedger)
import           Logging            (configureLogging)
import           SystemWiring       as SW
import           TransportUDP       (startNodeComm)

import           Data.Aeson         (ToJSON)
import           Network.Socket     (HostName, PortNumber)
import qualified Prelude            as PL (read)
import           Protolude

defaultHost :: HostName
defaultHost  = "224.0.0.99"
defaultPort :: PortNumber
defaultPort  = 9160

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    []             -> doIt defaultPort                      defaultHost (defaultPort :: PortNumber)
    [httpPort,h,p] -> doIt (PL.read httpPort :: PortNumber) h           (PL.read p   :: PortNumber)
    xss            -> error (show xss)

doIt :: PortNumber -> HostName -> PortNumber -> IO ()
doIt httpPort host port = do
  configureLogging
  (cd, cc) <- initializeWiring
  startNodeComm host port
                (CI.recFromConsensusNodes cc) (CI.getMsgToSendToConsensusNodes cc) (CI.sendToConsensusNodes cc)
  commandReceiver "0.0.0.0" httpPort (SW.listEntries cd) (SW.addEntry cd)

initializeWiring :: IO (SystemWiring LedgerImpl, ConsensusCommunicationWiring)
initializeWiring = do
  ledgerState <- newMVar genesisLedger
  commMV <- newEmptyMVar
  let iv   = Main.isValid ledgerState
      send = putMVar commMV
  return ( SystemWiring
             (Main.listEntries ledgerState)
             (Main.addEntry ledgerState send)
         , ConsensusCommunicationWiring
             (CI.recFromConsensusNodes' iv)
             (takeMVar commMV) -- getMsgToSendToConsensusNodes
             send              -- sendToConsensusNodes
         )

listEntries :: (ToJSON l, Ledger l) => MVar l -> Maybe Int -> IO (Maybe l)
listEntries ledger i = withMVar ledger $ \l -> return (Ledger.listEntries l i)

addEntry :: Ledger l => MVar l -> SendToConsensusNodes -> EData -> IO (Text, Text, Text)
addEntry l s d =
  withMVar l $ \l' -> do
  (i, ts, h) :: (EIndex, ETimestamp, EHash) <- CI.addEntry l' s d
  return (show i, show ts, show h)

isValid :: Ledger l => MVar l -> EIndex -> ETimestamp -> EData -> EHash -> IO (Maybe Text)
isValid l i t d h = withMVar l $ \l' -> return (isValidEntryData l' i t d h)
