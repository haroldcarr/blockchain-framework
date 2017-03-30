{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module LedgerImpl
  ( mkLedger
  , genesisLedger
  , genesisLedgerEntry
  , generateNextLedgerEntry
  , generateNextLedgerEntryInfo
  , getLastCommittedEntry
  , addLedgerEntry
  , calculateHash
  , isValidLedgerEntry
  , isValidLedger
  , ETimestamp
  , EData
  , listEntries
  , isValidEntry
  , isValidEntry'
  , getEntry
    -- protected
  , LedgerEntryImpl (..)
  , LedgerImpl
  )
where

import           Ledger
import           Util

import           Control.Applicative   ((<|>))
import           Crypto.Hash.SHA256    (hash)
import           Data.Aeson
import           Data.Aeson.Types      (typeMismatch)
import           Data.ByteString       as BS (concat)
import           Data.ByteString.Char8 as BSC8 (pack)
import           Data.Maybe            (fromMaybe)
import           Data.Sequence         as S (Seq, drop, index, length,
                                             singleton, (<|))

------------------------------------------------------------------------------

type LedgerImpl = Seq LedgerEntryImpl

data LedgerEntryImpl =
  LedgerEntryImpl { eindex        :: ! EIndex
                  , epreviousHash :: ! EHash
                  , etimestamp    :: ! ETimestamp
                  , edata         :: ! EData
                  , ehash         :: ! EHash
                  } deriving (Eq, Show)

------------------------------------------------------------------------------

instance Ledger (Seq LedgerEntryImpl) where
  listEntries ledger i =
    case i of
      -- return all entries
      Nothing -> Just ledger
      -- return the single entry (as a one-element list)
      Just i' -> -- Just (S.singleton (S.index ledger i'))
        case getEntry ledger i' of
          Nothing -> Nothing
          Just e  -> Just (S.singleton e)
  genNextEntry ledger ts d =
    let (LedgerEntryImpl i _ _ _ h) = generateNextLedgerEntry ledger ts d
    in (i, h)

instance LedgerEntry LedgerEntryImpl (Seq LedgerEntryImpl) where
  getEntry ledger i   = if i < S.length ledger then Just (S.index ledger i) else Nothing
  isValidEntry ledger = isValidLedgerEntry (S.index ledger 0)

------------------------------------------------------------------------------

data LedgerEntry2 =
  LedgerEntry2 { bindex2 :: ! EIndex } deriving (Eq, Show)

instance Ledger [LedgerEntry2] where
  listEntries ledger i =
    case i of
      -- return all entries
      Nothing -> Just ledger
      -- return the single entry (as a one-element list)
      Just i' -> Just [ledger !! i']
  genNextEntry _ _ _ = undefined

instance LedgerEntry LedgerEntry2 [LedgerEntry2] where
  getEntry ledger i = if i < Prelude.length ledger
                        then Just (ledger !! i)
                        else Nothing
  isValidEntry ledger ledgerEntry =
    if bindex2 (head ledger) == bindex2 ledgerEntry then Nothing else Just "no"

------------------------------------------------------------------------------

calculateHash :: EIndex -> EHash -> ETimestamp -> EData -> EHash
calculateHash i p t d = hash (BS.concat [BSC8.pack (show i), p, BSC8.pack (show t), d])

calculateHashForLedgerEntry :: LedgerEntryImpl -> EHash
calculateHashForLedgerEntry b = calculateHash (eindex b) (epreviousHash b) (etimestamp b) (edata b)

genesisLedgerEntry :: LedgerEntryImpl
genesisLedgerEntry =
  let i  = 0
      ph = "0"
      t  = "2017-03-05 10:49:02.084473 PST"
      d  = "GENESIS BLOCK"
      h  = calculateHash i ph t d
  in LedgerEntryImpl i ph t d h

genesisLedger :: Seq LedgerEntryImpl
genesisLedger  = S.singleton genesisLedgerEntry

getLastCommittedEntry :: Seq LedgerEntryImpl -> LedgerEntryImpl
getLastCommittedEntry es = fromMaybe (error "getLastCommittedEntry") (getEntry es 0)

mkLedger :: LedgerEntryImpl -> Seq LedgerEntryImpl
mkLedger = S.singleton

generateNextLedgerEntryInfo :: LedgerImpl -> ETimestamp -> EData -> (EIndex, EHash, ETimestamp, EData, EHash)
generateNextLedgerEntryInfo ledger tstamp edata0 =
  let prev = getLastCommittedEntry ledger
      i    = eindex prev + 1
      ph   = ehash prev
  in (i, ph, tstamp, edata0, calculateHash i ph tstamp edata0)

generateNextLedgerEntry :: LedgerImpl -> ETimestamp -> EData -> LedgerEntryImpl
generateNextLedgerEntry ledger tstamp edata0 =
  let (i, ph, _, _, h) = generateNextLedgerEntryInfo ledger tstamp edata0
  in LedgerEntryImpl i ph tstamp edata0 h

-- | Returns Nothing if valid.
isValidLedgerEntry :: LedgerEntryImpl -> LedgerEntryImpl -> Maybe String
isValidLedgerEntry previousLedgerEntry newLedgerEntry
  | eindex previousLedgerEntry + 1             /= eindex newLedgerEntry        = Just "invalid bindex"
  | ehash previousLedgerEntry                  /= epreviousHash newLedgerEntry = Just "invalid previousHash"
  | calculateHashForLedgerEntry newLedgerEntry /= ehash newLedgerEntry         = Just "invalid bhash"
  | otherwise                                                                  = Nothing

-- | Returns Nothing if valid.
isValidLedger :: Seq LedgerEntryImpl -> Maybe String
isValidLedger l
  | S.length l >= 2 = isValidLedgerEntry (S.index l 1) (S.index l 0) <|> isValidLedger (S.drop 1 l)
  | S.length l == 1 = Nothing
  | otherwise       = Just "empty ledger"

addLedgerEntry :: LedgerEntryImpl -> Seq LedgerEntryImpl -> Seq LedgerEntryImpl
addLedgerEntry = (<|)

isValidEntry' :: LedgerImpl -> EIndex -> ETimestamp -> EData -> EHash -> Maybe String
isValidEntry' l ei et ed eh =
  let prevE = S.index l (ei - 1)
      nle = LedgerEntryImpl ei (ehash prevE) et ed eh
  in isValidEntry l nle

------------------------------------------------------------------------------

-- https://github.com/bos/aeson/issues/187

instance ToJSON LedgerEntryImpl where
  toJSON (LedgerEntryImpl i ph t d h) =
    object [ "eindex"       .=                i
           , "previousHash" .= encodeToText64 ph
           , "timestamp"    .= encodeToText64 t
           , "edata"        .= encodeToText64 d
           , "ehash"        .= encodeToText64 h
           ]

instance FromJSON LedgerEntryImpl where
  parseJSON (Object o) =
    LedgerEntryImpl <$>  o .: "eindex"
                    <*> (o .: "previousHash" >>= decodeFromText64)
                    <*> (o .: "timestamp"    >>= decodeFromText64)
                    <*> (o .: "edata"        >>= decodeFromText64)
                    <*> (o .: "ehash"        >>= decodeFromText64)
  parseJSON invalid    = typeMismatch "LedgerEntry" invalid
