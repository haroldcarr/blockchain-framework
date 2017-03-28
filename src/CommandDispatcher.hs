{-# LANGUAGE OverloadedStrings #-}

module CommandDispatcher
  ( HandleConsensusMessage
  , CommandDispatcher (CommandDispatcher)
  )
where

import           Consensus  (HandleConsensusMessage)
import           Ledger     (EData)
import           LedgerImpl (Ledger, LedgerEntry)

data CommandDispatcher =
  CommandDispatcher
  -- CONSENSUS
  {-  handleConsensusMessage       :: -} HandleConsensusMessage
  {-, getMsgToSendToConsensusNodes :: -} (IO EData)
  {-, sendToConsensusNodes         :: -} (EData       -> IO ())
  -- BLOCKCHAIN
    -- Nothing: return all; Just i: return block at index i
  {-, listBlocks                   :: -} (Maybe Int   -> IO (Maybe Ledger))
  {-, addBlock                     :: -} (EData       -> IO LedgerEntry)  -- TODO : split into Blockchain and Consensus ops
  {-, isValid                      :: -} (LedgerEntry -> IO (Maybe String))
