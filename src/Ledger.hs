{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Ledger
  ( EIndex
  , EHash
  , ETimestamp
  , EData
  , LedgerOperations (LedgerOperations)
  )
where

import           Data.ByteString (ByteString)

type EIndex     = Int
type EHash      = ByteString
type ETimestamp = ByteString
type EData      = ByteString

class LedgerTC a where
class LedgerEntryTC a where

data LedgerOperations l e =
  LedgerOperations
    -- listEntries : Nothing: return all; Just i: return block at index i
    (LedgerTC l => Maybe Int   -> IO (Maybe l))
    -- isValidEntry : Nothing: valid; Just reason: not valid with reason
    (LedgerEntryTC e => e -> IO (Maybe String))
