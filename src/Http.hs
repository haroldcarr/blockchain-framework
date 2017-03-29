{-# LANGUAGE OverloadedStrings #-}

module Http
  ( commandReceiver
  , listEntriesReq
  )
where

import           Ledger                (EData)
import           Logging               (http)
import           SystemWiring          (AddEntry, ListEntries)

import           Control.Applicative   ((<|>))
import           Control.Monad.Trans   (liftIO)
import           Data.Aeson            (ToJSON, encode)
import           Data.ByteString.Char8 as BSC8 (unpack)
import           Data.ByteString.Lazy  (toStrict)
import           Data.Monoid           ((<>))
import           Network.Socket        (HostName, PortNumber)
import           Snap.Core             (Snap, getParam, ifTop, route, writeBS)
import           Snap.Http.Server      (Config, ConfigLog (ConfigNoLog),
                                        setAccessLog, setErrorLog, setPort,
                                        simpleHttpServe)
import           Snap.Internal.Core    (MonadSnap)
import           System.Log.Logger     (infoM)
import           Text.Read             (readMaybe)

commandReceiver :: (Show e, ToJSON e, ToJSON l)
                => HostName -> PortNumber
                -> ListEntries l
                -> AddEntry e
                -> IO ()
commandReceiver host port listEntries addEntry = do
  let config = setErrorLog ConfigNoLog . setAccessLog ConfigNoLog $ setPort (fromEnum port) mempty :: Config Snap ()
  simpleHttpServe config $
    ifTop (writeBS "hello world") <|>
    route [ ("entries/:i",   listEntriesReq listEntries)
          , ("addEntry/:bd", addEntryReq host port addEntry)
          ]

listEntriesReq :: (ToJSON a, Read i, MonadSnap m)
               => (Maybe i -> IO a)
               -> m ()
listEntriesReq listEntries = do
  i <- getParam "i"
  maybe (writeBS "must specify index")
        (\i' -> case readMaybe (BSC8.unpack i') of
                  Nothing -> writeBS "index must be an int"
                  justI   -> do entries <- liftIO (listEntries justI)
                                writeBS (toStrict (encode entries)))
        i

addEntryReq :: (ToJSON a, Show a, MonadSnap m)
            => HostName
            -> PortNumber
            -> (EData -> IO a)
            -> m ()
addEntryReq host port addEntry = do
  bd <- getParam "bd"
  maybe (writeBS "must specify data")
        (\bd' -> do newEntry <- liftIO (addEntry bd')
                    liftIO (infoM http ("http: addEntryReq: " <> host <> " " <> show port <> " " <> show newEntry))
                    writeBS (toStrict (encode newEntry)))
        bd

