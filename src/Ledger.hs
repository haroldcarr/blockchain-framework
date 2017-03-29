{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Ledger
  ( EIndex
  , EHash
  , ETimestamp
  , EData
  , Ledger (..)
  , LedgerEntry (..)
  )
where

import           Data.ByteString (ByteString)

type EIndex     = Int
type EHash      = ByteString
type ETimestamp = ByteString
type EData      = ByteString

class Ledger a where
  -- | index: Nothing: return all; Just i: return block at index i
  --   output: Nothing if index out of range
  listEntries :: a -> Maybe Int -> Maybe a

class LedgerEntry e l where
  -- | isValidEntry : Nothing: valid; Just reason: not valid with reason.
  isValidEntry :: Ledger l => l -> e   -> Maybe String
  -- | Nothing if index out of range.
  getEntry     :: Ledger l => l -> Int -> Maybe e

