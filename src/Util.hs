{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Data.ByteString.Base64 as BS64 (decode, encode)
import           Prelude                (fail)
import           Protolude

encodeToText64   :: ByteString -> Text
encodeToText64    = decodeUtf8 . BS64.encode

decodeFromText64 :: (Monad m) => Text -> m ByteString
decodeFromText64  = either fail return . BS64.decode . encodeUtf8
