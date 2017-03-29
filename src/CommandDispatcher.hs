{-# LANGUAGE OverloadedStrings #-}

module CommandDispatcher
  ( HandleConsensusMessage
  , CommandDispatcher (CommandDispatcher)
  )
where

import           ConsensusImpl (HandleConsensusMessage)
import           Ledger        (EData, EHash, EIndex, ETimestamp)

data CommandDispatcher entry ledger =
  CommandDispatcher
    -- CONSENSUS
    -- handleConsensusMessage
    HandleConsensusMessage
    -- getMsgToSendToConsensusNodes
    (IO EData)
    -- sendToConsensusNodes
    (EData       -> IO ())
    -- BLOCKCHAIN
    -- listBlocks : Nothing: return all; Just i: return block at index i
    (Maybe Int   -> IO (Maybe ledger))
    -- TODO : split into Blockchain and Consensus ops
    -- addBlock
    (EData       -> IO entry)
    -- isValid
    (EIndex -> ETimestamp -> EData -> EHash -> IO (Maybe String))
