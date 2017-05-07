{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module ConsensusImpl
  ( AppendEntry (..)
  , AppendEntryResponse (..)
  , recFromConsensusNodes'
  , ConsensusCommunicationWiring (..)
  , RecFromConsensusNodes
  , RecFromConsensusNodes2
  , GetMsgToSendToConsensusNodes
  , SendToConsensusNodes
  , addEntry
  )
where

import           Ledger
import           Logging
import           Util

import           Data.Aeson           (FromJSON, ToJSON, Value (Object),
                                       decodeStrict, encode, object, parseJSON,
                                       toJSON, (.:), (.=))
import           Data.Aeson.Types     (typeMismatch)
import           Data.ByteString      as BS
import           Data.ByteString.Lazy as BSL (toStrict)
import           Network.Socket       (HostName, PortNumber)
import           Protolude
import           System.Log.Logger    (infoM)

data AppendEntry =
  AppendEntry { aetype      :: ! Text
              , aeindex     :: ! EIndex
              , aetimestamp :: ! ETimestamp
              , aedata      :: ! EData
              , aehash      :: ! EHash
              }
  deriving (Eq, Generic, Show)

data AppendEntryResponse =
  AppendEntryResponse { aeresponse :: ! Bool
                      , aerindex   :: ! (Maybe EIndex)
                      }
  deriving (Eq, Generic, Show)

instance ToJSON   AppendEntryResponse
instance FromJSON AppendEntryResponse

instance ToJSON AppendEntry where
  toJSON (AppendEntry t i ts d h) =
    object [ "aetype"      .= t
           , "aeindex"     .= i
           , "aetimestamp" .= encodeToText64 ts
           , "aedata"      .= encodeToText64 d
           , "aehash"      .= encodeToText64 h
           ]

instance FromJSON AppendEntry where
  parseJSON (Object o) =
    AppendEntry <$> (o .: "aetype")
                <*> (o .: "aeindex")
                <*> (o .: "aetimestamp" >>= decodeFromText64)
                <*> (o .: "aedata"      >>= decodeFromText64)
                <*> (o .: "aehash"      >>= decodeFromText64)
  parseJSON invalid    = typeMismatch "AppendEntry" invalid

------------------------------------------------------------------------------

addEntry :: Ledger l => l -> SendToConsensusNodes -> EData -> IO (EIndex, ETimestamp, EHash)
addEntry ledger sendToConsensusNodes0 edata0 = do
  let ts     = "fake timestamp"
      (i, h) = genNextEntry ledger ts  edata0
  -- send entry to verifiers
  sendToConsensusNodes0 (BSL.toStrict (encode (AppendEntry "AER" i ts edata0 h)))
  return (i, ts, h)

------------------------------------------------------------------------------
-- Communication

type GetMsgToSendToConsensusNodes = IO EData
type SendToConsensusNodes         = EData -> IO ()
type RecFromConsensusNodes        = IsValid -> RecFromConsensusNodes2
type RecFromConsensusNodes2       = HostName
                                 -> PortNumber
                                 -> SendToConsensusNodes
                                 -> ByteString
                                 -> IO ()
type IsValid                      = EIndex -> ETimestamp -> EData -> EHash -> IO (Maybe Text)

-- | This structure is just to conceptually group communicaiton functions.
-- It is not stricly necessary.
data ConsensusCommunicationWiring =
  ConsensusCommunicationWiring
    { recFromConsensusNodes        :: RecFromConsensusNodes2
    , getMsgToSendToConsensusNodes :: GetMsgToSendToConsensusNodes
    , sendToConsensusNodes         :: SendToConsensusNodes
    }

-- recFromConsensusNodes :: RecFromConsensusNodes
recFromConsensusNodes' :: RecFromConsensusNodes
recFromConsensusNodes' isValid host port sendToConsensusNodes' msg0 =
  if | BS.isInfixOf "\"aetype\":\"AER\"" msg0 -> do
         infoC host port "APPENDENTRY"
         case decodeStrict msg0 of
           Nothing ->     sendToConsensusNodes' (BSL.toStrict (encode (AppendEntryResponse False Nothing)))
           Just (AppendEntry _ aei aets aed aeh) -> do
             v <- isValid aei aets aed aeh
             case v of
               Nothing -> sendToConsensusNodes' (BSL.toStrict (encode (AppendEntryResponse True  (Just aei))))
               _       -> sendToConsensusNodes' (BSL.toStrict (encode (AppendEntryResponse False (Just aei))))
     | BS.isInfixOf "\"aeresponse\":" msg0 -> do
         infoC host port "APPENDENTRYRESPONSE"
         case decodeStrict msg0 of
           Just aer@(AppendEntryResponse _ _) -> infoC host port (show aer)
           Nothing                            -> infoC host port "AER NOT OK"
     | otherwise -> infoC host port ("handleMessage: unknown message: " <> show msg0)

------------------------------------------------------------------------------

infoC :: HostName -> PortNumber -> Text -> IO ()
infoC h p msg0 =
  infoM (toS consensus) (toS ("C " <> toS h <> ":" <> show p <> " " <> msg0))

