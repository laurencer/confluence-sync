module Network.XmlRpc.Base64 (
    encode,
    decode
) where

import           Data.ByteString
import qualified Data.ByteString.Base64 as B64

encode :: ByteString -> ByteString
encode = B64.encode

decode :: ByteString -> ByteString
decode = B64.decodeLenient
