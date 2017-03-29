{-# LANGUAGE OverloadedStrings #-}

module CommandDispatcher where

import           ConsensusImpl (RecFromConsensusNodes)
import           Ledger        (EData, EHash, EIndex, ETimestamp)

type GetMsgToSendToConsensusNodes = IO EData
type SendToConsensusNodes         = EData     -> IO ()
type ListEntries ledger           = Maybe Int -> IO (Maybe ledger)
type AddEntry entry               = EData     -> IO entry
type IsValid                      = EIndex    -> ETimestamp -> EData -> EHash -> IO (Maybe String)

data CommandDispatcher entry ledger =
  CommandDispatcher
    {
      -- CONSENSUS
      recFromConsensusNodes        :: RecFromConsensusNodes
    , getMsgToSendToConsensusNodes :: GetMsgToSendToConsensusNodes
    , sendToConsensusNodes         :: SendToConsensusNodes
      -- LEDGER
      -- Nothing: return all; Just i: return block at index i
    , listEntries                  :: ListEntries ledger
      -- TODO : split into Blockchain and Consensus ops
    , addEntry                     :: AddEntry entry
    , isValid                      :: IsValid
   }
