{-# LANGUAGE OverloadedStrings #-}

module LedgerImpl
  ( LedgerEntry (LedgerEntry)
  , Ledger
  , generateNextLedgerEntry
  , genesisLedgerEntry
    -- for testing
  , addLedgerEntry
  , bhash
  , calculateHash
  , isValidLedgerEntry
  , isValidLedger
  , Timestamp
  , BlockData
  )
where

import           Control.Applicative    ((<|>))
import           Crypto.Hash.SHA256     (hash)
import           Data.Aeson
import           Data.Aeson.Types       (typeMismatch)
import           Data.ByteString        as BS (ByteString, concat)
import           Data.ByteString.Base64 as BS64
import           Data.ByteString.Char8  as BSC8 (pack)
import           Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)

type Index      = Integer
type Hash       = ByteString
type Timestamp  = ByteString
type BlockData  = ByteString
type Ledger     = [LedgerEntry]

data LedgerEntry =
  LedgerEntry { bindex       :: ! Index
              , previousHash :: ! Hash
              , timestamp    :: ! Timestamp
              , bdata        :: ! BlockData
              , bhash        :: ! Hash
              } deriving (Eq, Show)

calculateHash :: Index -> Hash -> Timestamp -> BlockData -> Hash
calculateHash i p t d = hash (BS.concat [BSC8.pack (show i), p, BSC8.pack (show t), d])

calculateHashForLedgerEntry :: LedgerEntry -> Hash
calculateHashForLedgerEntry b = calculateHash (bindex b) (previousHash b) (timestamp b) (bdata b)

genesisLedgerEntry :: LedgerEntry
genesisLedgerEntry =
  let i  = 0
      ph = "0"
      t  = "2017-03-05 10:49:02.084473 PST"
      d  = "GENESIS BLOCK"
      h  = calculateHash i ph t d
  in LedgerEntry i ph t d h

generateNextLedgerEntry :: LedgerEntry -> Timestamp -> BlockData -> LedgerEntry
generateNextLedgerEntry previousLedgerEntry tstamp blockData =
  let i  = bindex previousLedgerEntry + 1
      ph = bhash previousLedgerEntry
  in LedgerEntry i ph tstamp blockData (calculateHash i ph tstamp blockData)

-- | Returns Nothing if valid.
isValidLedgerEntry :: LedgerEntry -> LedgerEntry -> Maybe String
isValidLedgerEntry previousLedgerEntry newLedgerEntry
  | bindex previousLedgerEntry + 1             /= bindex newLedgerEntry       = Just "invalid index"
  | bhash previousLedgerEntry                  /= previousHash newLedgerEntry = Just "invalid previousHash"
  | calculateHashForLedgerEntry newLedgerEntry /= bhash newLedgerEntry        = Just "invalid hash"
  | otherwise                                                                 = Nothing

-- | Returns Nothing if valid.
isValidLedger :: Ledger -> Maybe String
isValidLedger (b:pb:bs) = isValidLedgerEntry pb b <|> isValidLedger (pb:bs)
isValidLedger      [_]  = Nothing
isValidLedger       []  = Just "empty ledger"

addLedgerEntry :: LedgerEntry -> Ledger -> Ledger
addLedgerEntry e es = e : es

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

