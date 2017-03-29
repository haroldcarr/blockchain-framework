{-# LANGUAGE OverloadedStrings #-}

module Consensus
  ()
where

{-
data Consensus =
  Consensus
    -- handleConsensusMessage
    HandleConsensusMessage
    -- getMsgToSendToConsensusNodes
    (IO EData)
    -- sendToConsensusNodes
    (EData       -> IO ())
-}
