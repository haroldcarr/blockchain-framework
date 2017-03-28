{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Ledger
  ( EIndex
  , EHash
  , ETimestamp
  , EData
  )
where

import           Data.ByteString (ByteString)

type EIndex     = Int
type EHash      = ByteString
type ETimestamp = ByteString
type EData      = ByteString
