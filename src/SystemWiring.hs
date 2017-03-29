{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module SystemWiring where

import           Ledger     (EData, Ledger)

import           Data.Aeson (ToJSON)

type ListEntries ledger = (Ledger ledger, ToJSON ledger)
                       => Maybe Int -> IO (Maybe ledger)
type AddEntry    entry  = EData     -> IO (String, String, String)

data SystemWiring ledger entry =
  SystemWiring
    {
      -- LEDGER
      -- Nothing: return all; Just i: return block at index i
      listEntries :: ListEntries ledger
      -- TODO : split into Blockchain and Consensus ops
    , addEntry    :: AddEntry entry
   }
