{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module SendMessage where

import Foundation
import Yesod.Core
import Network.HTTP.Client
import Network.HTTP.Types
import Network.HTTP.Client.MultipartFormData
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client.TLS
import Data.Aeson
import Data.List
import Data.Maybe
import Data.Text
import Data.ByteString.Lazy
import qualified Data.ByteString as Data.ByteString.Internal

data DiscordMessageBody = DiscordMessageBody {
    _contents :: Maybe String,
    _embeds :: Maybe [DiscordEmbedBody]
} deriving (Show)
instance ToJSON DiscordMessageBody where
  toJSON (DiscordMessageBody { _contents = _contents, _embeds = _embeds }) =
    object [ "contents" .= _contents
           , "embeds"  .= _embeds
           ]
instance FromJSON DiscordMessageBody where
    parseJSON = withObject "DiscordMessageBody" $ \v -> DiscordMessageBody
        <$> v .: "contents"
        <*> v .: "embeds"

data DiscordEmbedBody = DiscordEmbedBody {
    _title :: String,
    _description :: String,
    _color :: Int,
    _fields :: Maybe [DiscordEmbedField]
} deriving (Show)
instance ToJSON DiscordEmbedBody where
  toJSON (DiscordEmbedBody { _title = _title, _description = _description, _color = _color, _fields = _fields }) =
    object [ "title" .= _title
           , "description"  .= _description
           , "color"  .= _color
           , "fields"  .= _fields
           ]
instance FromJSON DiscordEmbedBody where
    parseJSON = withObject "DiscordEmbedBody" $ \v -> DiscordEmbedBody
        <$> v .: "contents"
        <*> v .: "description"
        <*> v .: "color"
        <*> v .: "fields"

data DiscordEmbedField = DiscordEmbedField {
    _name :: String,
    _value :: String
} deriving (Show)
instance ToJSON DiscordEmbedField where
  toJSON (DiscordEmbedField { _name = _name, _value = _value }) =
    object [ "name" .= _name
           , "value"  .= _value
           ]
instance FromJSON DiscordEmbedField where
    parseJSON = withObject "DiscordEmbedField" $ \v -> DiscordEmbedField
        <$> v .: "name"
        <*> v .: "value"

postFormData :: Manager -> String -> RequestHeaders -> [Part] -> IO (BL.ByteString, Int)
postFormData manager url headers body = do
    initialRequest <- parseRequest $ "POST " ++ url
    let request = initialRequest { requestHeaders = headers }
    newReq <- formDataBody body request
    resp <- httpLbs newReq manager
    let status = statusCode $ responseStatus resp
    pure (responseBody resp, status)

createDiscordEmbedBody :: CompressVideoPayload -> Data.ByteString.Internal.ByteString
createDiscordEmbedBody CompressVideoPayload { title, description, tags } = do
    let tagField = DiscordEmbedField { _name = "tags", _value = tags }
        embedBody = DiscordEmbedBody { _title = title, _description = description, _color = 7888639, _fields = Just [tagField]}
    toStrict $ encode $ DiscordMessageBody { _contents = Nothing, _embeds = Just [embedBody] }

sendMessage :: CompressVideoPayload -> String -> String -> IO ()
sendMessage payload webhookUrl clipPath = do
    let headers = [("Content-Type", "multipart/form-data"), ("accept", "application/json")]
        body = [partBS "payload_json" (createDiscordEmbedBody payload), partFileSource "file1" clipPath]
    manager <- getGlobalManager
    (response, status) <- postFormData manager webhookUrl headers body
    print status
    print response
    return ()


-- {
--   "content": null,
--   "embeds": [
--     {
--       "title": "Rob is that you?",
--       "description": "That's not rob.",
--       "color": 7888639,
--       "fields": [
--         {
--           "name": "tags",
--           "value": "haha, funny, silly, running, rob"
--         }
--       ]
--     }
--   ],
--   "attachments": []
-- }