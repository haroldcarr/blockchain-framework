{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module ConsensusImpl
  ( AppendEntry (..)
  , AppendEntryResponse (..)
  , ConsensusMessage
  , recFromConsensusNodes
  , RecFromConsensusNodes
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
import           Data.ByteString.Lazy (toStrict)
import           Data.Monoid          ((<>))
import           GHC.Generics
import           Network.Socket       as N (HostName, PortNumber)
import           System.Log.Logger    (infoM)

data AppendEntry =
  AppendEntry { aetype      :: ! String
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

type ConsensusMessage = ByteString

type RecFromConsensusNodes  = HostName
                           -> PortNumber
                           -> (ByteString -> IO ())
                           -> (EIndex -> ETimestamp -> EData -> EHash -> IO (Maybe String))
                           -> ByteString
                           -> IO ()

recFromConsensusNodes :: RecFromConsensusNodes
recFromConsensusNodes host port sendToConsensusNodes isValid msg =
  if | BS.isInfixOf "\"aetype\":\"AER\"" msg -> do
         infoC host port "APPENDENTRY"
         case decodeStrict msg of
           Nothing ->     sendToConsensusNodes (toStrict (encode (AppendEntryResponse False Nothing)))
           Just (AppendEntry _ aei aets aed aeh) -> do
             v <- isValid aei aets aed aeh
             case v of
               Nothing -> sendToConsensusNodes (toStrict (encode (AppendEntryResponse True  (Just aei))))
               _       -> sendToConsensusNodes (toStrict (encode (AppendEntryResponse False (Just aei))))
     | BS.isInfixOf "\"aeresponse\":" msg -> do
         infoC host port "APPENDENTRYRESPONSE"
         case decodeStrict msg of
           Just aer@(AppendEntryResponse _ _) -> infoC host port (show aer)
           Nothing                            -> infoC host port "AER NOT OK"
     | otherwise -> infoC host port ("handleMessage: unknown message: " ++ show msg)

------------------------------------------------------------------------------

infoC :: HostName -> PortNumber -> String -> IO ()
infoC h p msg =
  infoM consensus ("C " <> h <> ":" <> show p <> " " <> msg)
