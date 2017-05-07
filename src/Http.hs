{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Http
  ( commandReceiver
  , listEntriesReq
  )
where

import           Ledger                (EData)
import           Logging               (http)

import           Data.Aeson            (ToJSON, encode)
import           Data.ByteString.Char8 as BSC8 (unpack)
import           Network.Socket        (HostName, PortNumber)
import           Protolude
import           Snap.Core             (Snap, getParam, ifTop, route, writeBS)
import           Snap.Http.Server      (Config, ConfigLog (ConfigNoLog),
                                        setAccessLog, setErrorLog, setPort,
                                        simpleHttpServe)
import           Snap.Internal.Core    (MonadSnap)
import           System.Log.Logger     (infoM)

commandReceiver :: (Show e, ToJSON e, ToJSON l, Read i)
                => HostName -> PortNumber
                -> (Maybe i -> IO l)
                -> (EData -> IO e)
                -> IO ()
commandReceiver host port listEntries addEntry = do
  let config = setErrorLog ConfigNoLog . setAccessLog ConfigNoLog $ setPort (fromEnum port) mempty :: Config Snap ()
  simpleHttpServe config $
    ifTop (writeBS "hello world") <|>
    route [ ("entries/:i",   listEntriesReq listEntries)
          , ("addEntry/:ed", addEntryReq host port addEntry)
          ]

listEntriesReq :: (ToJSON l, Read i, MonadSnap m)
               => (Maybe i -> IO l)
               -> m ()
listEntriesReq listEntries = do
  i <- getParam "i"
  maybe (writeBS "must specify index")
        (\i' -> case readMaybe (BSC8.unpack i') of
                  Nothing -> writeBS "index must be an int"
                  justI   -> do entries <- liftIO (listEntries justI)
                                writeBS (toS (encode entries)))
        i

addEntryReq :: (ToJSON e, Show e, MonadSnap m)
            => HostName
            -> PortNumber
            -> (EData -> IO e)
            -> m ()
addEntryReq host port addEntry = do
  ed <- getParam "ed"
  maybe (writeBS "must specify data")
        (\ed' -> do newEntry <- liftIO (addEntry ed')
                    liftIO (infoM (toS http) ("http: addEntryReq: " <> host <> " " <> show port <> " " <> show newEntry))
                    writeBS (toS (encode newEntry)))
        ed

