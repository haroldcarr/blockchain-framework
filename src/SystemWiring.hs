{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module SystemWiring where

import           Ledger     (EData, Ledger)

import           Data.Aeson (ToJSON)
import           Protolude

type ListEntries ledger = (Ledger ledger, ToJSON ledger)
                       => Maybe Int -> IO (Maybe ledger)
type AddEntry           = EData     -> IO (Text, Text, Text)

data SystemWiring ledger =
  SystemWiring
    {
      -- LEDGER
      -- Nothing: return all; Just i: return block at index i
      listEntries :: ListEntries ledger
      -- TODO : split into Blockchain and Consensus ops
    , addEntry    :: AddEntry
   }
