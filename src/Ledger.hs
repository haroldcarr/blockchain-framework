{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Ledger where

import           Protolude

type EIndex     = Int
type EHash      = ByteString
type ETimestamp = ByteString
type EData      = ByteString

class Ledger l where
  -- | input : Nothing: return all; Just i: return block at index i
  --   output: Nothing if index out of range
  listEntries      :: l -> Maybe Int -> Maybe l
  genNextEntry     :: l -> ETimestamp -> EData -> (EIndex, EHash)
  isValidEntryData :: l -> EIndex -> ETimestamp -> EData -> EHash -> Maybe Text

class LedgerEntry e l where
  -- | isValidEntry : Nothing: valid; Just reason: not valid with reason.
  isValidEntry :: Ledger l => l -> e   -> Maybe Text
  -- | Nothing if index out of range.
  getEntry     :: Ledger l => l -> Int -> Maybe e
