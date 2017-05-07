{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Logging
  ( mainProgram
  , http
  , consensus
  , consensusFollower
  , consensusLeader
  , configureLogging
  )
where

import           Protolude
import           System.Log.Logger (Priority (INFO), setLevel,
                                    updateGlobalLogger)

mainProgram, http, consensus, consensusFollower, consensusLeader :: Text
mainProgram       = "MAIN"
http              = "HTTP"
consensus         = "Consensus"
consensusFollower = "Consensus.Follower"
consensusLeader   = "Consensus.Leader"

configureLogging :: IO ()
configureLogging  = do
  updateGlobalLogger (toS mainProgram)       (setLevel INFO)
  updateGlobalLogger (toS http)              (setLevel INFO)
  updateGlobalLogger (toS consensus)         (setLevel INFO)
  updateGlobalLogger (toS consensusFollower) (setLevel INFO)
  updateGlobalLogger (toS consensusLeader)   (setLevel INFO)
