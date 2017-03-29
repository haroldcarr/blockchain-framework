{-# LANGUAGE OverloadedStrings #-}

module SystemWiring where

import           Ledger (EData)

type ListEntries ledger           = Maybe Int -> IO (Maybe ledger)
type AddEntry entry               = EData     -> IO entry

data SystemWiring entry ledger =
  SystemWiring
    {
      -- LEDGER
      -- Nothing: return all; Just i: return block at index i
      listEntries :: ListEntries ledger
      -- TODO : split into Blockchain and Consensus ops
    , addEntry    :: AddEntry entry
   }
