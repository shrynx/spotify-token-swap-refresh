module Utils
  ( byteToText
  , textToByte
  , stringToByte
  , stringToText
  )
where

import           Relude
import qualified Data.ByteString.Char8         as B
import qualified Data.Text                     as T

byteToText :: ByteString -> Text
byteToText = T.pack . B.unpack

textToByte :: Text -> ByteString
textToByte = B.pack . T.unpack

stringToByte :: String -> ByteString
stringToByte = B.pack

stringToText :: String -> Text
stringToText = T.pack
