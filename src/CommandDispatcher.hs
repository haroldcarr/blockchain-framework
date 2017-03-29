{-# LANGUAGE OverloadedStrings #-}

module CommandDispatcher where

import           Ledger          (EData, EHash, EIndex, ETimestamp)

import           Data.ByteString as BS
import           Network.Socket  as N (HostName, PortNumber)

type RecFromConsensusNodes  = IsValid -> RecFromConsensusNodes2
type RecFromConsensusNodes2 = HostName
                           -> PortNumber
                           -> SendToConsensusNodes
                           -> ByteString
                           -> IO ()
type GetMsgToSendToConsensusNodes = IO EData
type SendToConsensusNodes         = EData     -> IO ()
type ListEntries ledger           = Maybe Int -> IO (Maybe ledger)
type AddEntry entry               = EData     -> IO entry
type IsValid                      = EIndex    -> ETimestamp -> EData -> EHash -> IO (Maybe String)

data CommandDispatcher entry ledger =
  CommandDispatcher
    {
      -- CONSENSUS
      recFromConsensusNodes        :: RecFromConsensusNodes2
    , getMsgToSendToConsensusNodes :: GetMsgToSendToConsensusNodes
    , sendToConsensusNodes         :: SendToConsensusNodes
      -- LEDGER
      -- Nothing: return all; Just i: return block at index i
    , listEntries                  :: ListEntries ledger
      -- TODO : split into Blockchain and Consensus ops
    , addEntry                     :: AddEntry entry
   }
