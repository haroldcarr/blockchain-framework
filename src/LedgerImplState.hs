module LedgerImplState
  ( initialLedgerImplState
  )
where

import           LedgerImpl              (Ledger, genesisLedgerEntry)

import           Control.Concurrent.MVar (MVar, newMVar)

initialLedgerImplState :: IO (MVar Ledger)
initialLedgerImplState = newMVar [genesisLedgerEntry]
