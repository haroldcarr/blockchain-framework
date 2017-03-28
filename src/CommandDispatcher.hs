{-# LANGUAGE OverloadedStrings #-}

module CommandDispatcher
  ( HandleConsensusMessage
  , CommandDispatcher (CommandDispatcher)
  )
where

import           Consensus  (HandleConsensusMessage)
import           LedgerImpl (BlockData, Ledger, LedgerEntry)

data CommandDispatcher =
  CommandDispatcher
  -- CONSENSUS
  {-  handleConsensusMessage       :: -} HandleConsensusMessage
  {-, getMsgToSendToConsensusNodes :: -} (IO BlockData)
  {-, sendToConsensusNodes         :: -} (BlockData   -> IO ())
  -- BLOCKCHAIN
    -- Nothing: return all; Just i: return block at index i
  {-, listBlocks                   :: -} (Maybe Int   -> IO (Maybe Ledger))
  {-, addBlock                     :: -} (BlockData   -> IO LedgerEntry)  -- TODO : split into Blockchain and Consensus ops
  {-, isValid                      :: -} (LedgerEntry -> IO (Maybe String))
