{-# LANGUAGE OverloadedStrings #-}

module TransportUDP
  (startNodeComm)
where

import           CommandDispatcher         hiding (getMsgToSendToConsensusNodes,
                                            recFromConsensusNodes,
                                            sendToConsensusNodes)
import           Ledger                    (EData)
import           Logging                   (consensusFollower)

import           Control.Concurrent        (forkIO)
import           Data.Monoid               ((<>))
import           Network.Multicast         as NM (multicastReceiver,
                                                  multicastSender)
import           Network.Socket            as N (HostName, PortNumber, SockAddr,
                                                 Socket)
import           Network.Socket.ByteString as N (recvFrom, sendTo)
import           System.Log.Logger         (infoM)

startNodeComm :: HostName -> PortNumber
              -> RecFromConsensusNodes2
              -> GetMsgToSendToConsensusNodes
              -> SendToConsensusNodes
              -> IO ()
startNodeComm host port recFromConsensusNodes getMsgToSendToConsensusNodes sendToConsensusNodes = do
  _ <- infoN host port "startNodeComm: ENTER"
  (sendSock, sendAddr) <- multicastSender host port
  recSock <- multicastReceiver host port
  forkIO $ send host port sendSock sendAddr getMsgToSendToConsensusNodes
  forkIO $ rec host port recSock sendSock sendAddr recFromConsensusNodes sendToConsensusNodes
  infoN host port "startNodeComm: EXIT"
  return ()

rec :: HostName -> PortNumber
    -> Socket -> Socket -> SockAddr
    -> RecFromConsensusNodes2
    -> SendToConsensusNodes
    -> IO b
rec host port recSock sendSock sendAddr recFromConsensusNodes sendToConsensusNodes = do
  infoN host port "rec: waiting"
  (msg,addr) <- N.recvFrom recSock 1024
  infoN host port  ("rec: from: " <> show addr <> " " <> show msg)
  recFromConsensusNodes host port sendToConsensusNodes msg
  rec host port recSock sendSock sendAddr recFromConsensusNodes sendToConsensusNodes

-- Read from sendToConsensusNodes and broadcast
send :: HostName -> PortNumber -> Socket -> SockAddr -> IO EData -> IO () -- TODO ByteString
send host port sock addr getMsgsToSendToConsensusNodes = do
  infoN host port "send: waiting"
  msg <- getMsgsToSendToConsensusNodes
  infoN host port ("send: " ++ show msg)
  sendTo sock msg addr
  send host port sock addr getMsgsToSendToConsensusNodes

infoN :: HostName -> PortNumber -> String -> IO Int
infoN h p msg = do
  infoM consensusFollower ("T " <> h <> ":" <> show p <> " " <> msg)
  return 1 -- to match sendTo

