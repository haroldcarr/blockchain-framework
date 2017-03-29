module Util where

import           Data.ByteString        as BS (ByteString)
import           Data.ByteString.Base64 as BS64 (decode, encode)
import           Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)

encodeToText64   :: ByteString -> Text
encodeToText64    = decodeUtf8 . BS64.encode

decodeFromText64 :: (Monad m) => Text -> m ByteString
decodeFromText64  = either fail return . BS64.decode . encodeUtf8
