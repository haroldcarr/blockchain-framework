{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TransportUDP
  (startNodeComm)
where

import           ConsensusImpl             (GetMsgToSendToConsensusNodes,
                                            RecFromConsensusNodes2,
                                            SendToConsensusNodes)
import           Logging                   (consensusFollower)

import           Network.Multicast         as NM (multicastReceiver,
                                                  multicastSender)
import           Network.Socket            as N (HostName, PortNumber, SockAddr,
                                                 Socket)
import           Network.Socket.ByteString as N (recvFrom, sendTo)
import           Protolude
import           System.Log.Logger         (infoM)

startNodeComm :: HostName -> PortNumber
              -> RecFromConsensusNodes2
              -> GetMsgToSendToConsensusNodes
              -> SendToConsensusNodes
              -> IO ()
startNodeComm host port recFromConsensusNodes getMsgToSendToConsensusNodes sendToConsensusNodes = do
  infoN host port "startNodeComm: ENTER"
  (sendSock, sendAddr) <- multicastSender host port
  recSock <- multicastReceiver host port
  forkIO $ send host port sendSock sendAddr getMsgToSendToConsensusNodes
  forkIO $ rec host port recSock sendSock sendAddr recFromConsensusNodes sendToConsensusNodes
  infoN host port "startNodeComm: EXIT"
  return ()

rec :: HostName -> PortNumber -> Socket -> Socket -> SockAddr
    -> RecFromConsensusNodes2 -> SendToConsensusNodes
    -> IO b
rec host port recSock sendSock sendAddr recFromConsensusNodes sendToConsensusNodes = do
  infoN host port "rec: waiting"
  (msg0,addr) <- N.recvFrom recSock 1024
  infoN host port  ("rec: from: " <> show addr <> " " <> show msg0)
  recFromConsensusNodes host port sendToConsensusNodes msg0
  rec host port recSock sendSock sendAddr recFromConsensusNodes sendToConsensusNodes

send :: HostName -> PortNumber -> Socket -> SockAddr
     -> GetMsgToSendToConsensusNodes
     -> IO ()
send host port sock addr getMsgToSendToConsensusNodes = do
  infoN host port "send: waiting"
  msg0 <- getMsgToSendToConsensusNodes
  infoN host port ("send: " <> show msg0)
  sendTo sock msg0 addr
  send host port sock addr getMsgToSendToConsensusNodes

infoN :: HostName -> PortNumber -> Text -> IO Int
infoN h p msg0 = do
  infoM (toS consensusFollower) (toS ("T " <> toS h <> ":" <> show p <> " " <> msg0))
  return 1 -- to match sendTo

