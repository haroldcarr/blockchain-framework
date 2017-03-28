{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module LedgerImpl
  ( LedgerEntry (LedgerEntry)
  , Ledger
  , mkLedger
  , genesisLedger
  , genesisLedgerEntry
  , generateNextLedgerEntry
  , getLastCommittedEntry
  , getEntry
  , addLedgerEntry
  , bhash
  , calculateHash
  , isValidLedgerEntry
  , isValidLedger
  , ETimestamp
  , EData
  , LedgerTC, listEntries
  , LedgerEntryTC, isValidEntry, getEntry'
  )
where

import           Ledger

import           Control.Applicative    ((<|>))
-- import           Control.Concurrent.MVar
-- import           Control.Lens            (element, (^?))
import           Crypto.Hash.SHA256     (hash)
import           Data.Aeson
import           Data.Aeson.Types       (typeMismatch)
import           Data.ByteString        as BS (ByteString, concat)
import           Data.ByteString.Base64 as BS64 (decode, encode)
import           Data.ByteString.Char8  as BSC8 (pack)
import           Data.Sequence          as S (Seq, drop, index, length,
                                              singleton, (<|))
import           Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)


type Ledger  = Seq LedgerEntry

data LedgerEntry =
  LedgerEntry { bindex       :: ! EIndex
              , previousHash :: ! EHash
              , timestamp    :: ! ETimestamp
              , bdata        :: ! EData
              , bhash        :: ! EHash
              } deriving (Eq, Show)

calculateHash :: EIndex -> EHash -> ETimestamp -> EData -> EHash
calculateHash i p t d = hash (BS.concat [BSC8.pack (show i), p, BSC8.pack (show t), d])

calculateHashForLedgerEntry :: LedgerEntry -> EHash
calculateHashForLedgerEntry b = calculateHash (bindex b) (previousHash b) (timestamp b) (bdata b)

genesisLedgerEntry :: LedgerEntry
genesisLedgerEntry =
  let i  = 0
      ph = "0"
      t  = "2017-03-05 10:49:02.084473 PST"
      d  = "GENESIS BLOCK"
      h  = calculateHash i ph t d
  in LedgerEntry i ph t d h

genesisLedger :: Seq LedgerEntry
genesisLedger  = S.singleton genesisLedgerEntry

getEntry :: Seq LedgerEntry -> Int -> Maybe LedgerEntry
getEntry es i
  | i < S.length es = Just (S.index es i)
  | otherwise       = Nothing

getLastCommittedEntry :: Seq LedgerEntry -> LedgerEntry
getLastCommittedEntry es = case getEntry es 0 of
  Nothing -> error "getLastCommittedEntry"
  Just e  -> e

mkLedger :: LedgerEntry -> Seq LedgerEntry
mkLedger = S.singleton

generateNextLedgerEntry :: LedgerEntry -> ETimestamp -> EData -> LedgerEntry
generateNextLedgerEntry previousLedgerEntry tstamp blockData =
  let i  = bindex previousLedgerEntry + 1
      ph = bhash previousLedgerEntry
  in LedgerEntry i ph tstamp blockData (calculateHash i ph tstamp blockData)

-- | Returns Nothing if valid.
isValidLedgerEntry :: LedgerEntry -> LedgerEntry -> Maybe String
isValidLedgerEntry previousLedgerEntry newLedgerEntry
  | bindex previousLedgerEntry + 1             /= bindex newLedgerEntry       = Just "invalid bindex"
  | bhash previousLedgerEntry                  /= previousHash newLedgerEntry = Just "invalid previousHash"
  | calculateHashForLedgerEntry newLedgerEntry /= bhash newLedgerEntry        = Just "invalid bhash"
  | otherwise                                                                 = Nothing

-- | Returns Nothing if valid.
isValidLedger :: Ledger -> Maybe String
isValidLedger l
  | S.length l >= 2 = isValidLedgerEntry (S.index l 1) (S.index l 0) <|> isValidLedger (S.drop 1 l)
  | S.length l == 1 = Nothing
  | otherwise       = Just "empty ledger"

addLedgerEntry :: LedgerEntry -> Ledger -> Ledger
addLedgerEntry = (<|)

------------------------------------------------------------------------------

-- https://github.com/bos/aeson/issues/187

instance ToJSON LedgerEntry where
  toJSON (LedgerEntry i ph t d h) =
    object [ "bindex"       .=                i
           , "previousHash" .= encodeToText64 ph
           , "timestamp"    .= encodeToText64 t
           , "bdata"        .= encodeToText64 d
           , "bhash"        .= encodeToText64 h
           ]

instance FromJSON LedgerEntry where
  parseJSON (Object o) =
    LedgerEntry <$>  o .: "bindex"
                <*> (o .: "previousHash" >>= decodeFromText64)
                <*> (o .: "timestamp"    >>= decodeFromText64)
                <*> (o .: "bdata"        >>= decodeFromText64)
                <*> (o .: "bhash"        >>= decodeFromText64)
  parseJSON invalid    = typeMismatch "LedgerEntry" invalid

encodeToText64   :: ByteString -> Text
encodeToText64    = decodeUtf8 . BS64.encode

decodeFromText64 :: (Monad m) => Text -> m ByteString
decodeFromText64  = either fail return . BS64.decode . encodeUtf8

------------------------------------------------------------------------------

class LedgerTC a where
  -- Nothing: return all; Just i: return block at index i
  listEntries :: a -> Maybe Int -> Maybe a

class LedgerEntryTC e l where
  -- isValidEntry : Nothing: valid; Just reason: not valid with reason
  isValidEntry :: LedgerTC l => l -> e -> Maybe String
  getEntry'    :: LedgerTC l => l -> Int -> Maybe e

instance (LedgerTC (Seq LedgerEntry)) where
  listEntries ledger i =
    case i of
      -- return all entries
      Nothing -> Just ledger
      -- return the single entry (as a one-element list)
      Just i' -> -- Just (S.singleton (S.index ledger i'))
        case getEntry' ledger i' of
          Nothing -> Nothing
          Just e  -> Just (S.singleton e)

instance (LedgerEntryTC LedgerEntry (Seq LedgerEntry)) where
  getEntry' ledger i = if i < S.length ledger then Just (S.index ledger i) else Nothing
  isValidEntry ledger ledgerEntry = isValidLedgerEntry (S.index ledger 0) ledgerEntry

-----

type Ledger2 = [LedgerEntry2]

data LedgerEntry2 =
  LedgerEntry2 { bindex2 :: ! EIndex } deriving (Eq, Show)

instance (LedgerTC Ledger2) where
  listEntries ledger i =
    case i of
      -- return all entries
      Nothing -> Just ledger
      -- return the single entry (as a one-element list)
      Just i' -> Just [(ledger !! i')]

instance (LedgerEntryTC LedgerEntry2 Ledger2) where
  getEntry' ledger i = if i < Prelude.length ledger
                              then Just (ledger !! i)
                              else Nothing
  isValidEntry ledger ledgerEntry =
    if (bindex2 (ledger !! 0)) == (bindex2 ledgerEntry) then Nothing else Just "no"

