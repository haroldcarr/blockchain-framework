module LedgerImplState
  ( initialLedgerImplState
  )
where

import           LedgerImpl              (Ledger, genesisLedger)

import           Control.Concurrent.MVar (MVar, newMVar)

initialLedgerImplState :: IO (MVar Ledger)
initialLedgerImplState = newMVar genesisLedger
