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

postFormData :: Manager -> String -> RequestHeaders -> [Part] -> IO (BL.ByteString, Int)
postFormData manager url headers body = do
    initialRequest <- parseRequest $ "POST " ++ url
    let request = initialRequest { requestHeaders = headers }
    newReq <- formDataBody body request
    resp <- httpLbs newReq manager
    let status = statusCode $ responseStatus resp
    pure (responseBody resp, status)

sendMessage :: String -> String -> IO ()
sendMessage webhookUrl clipPath = do
    let headers = [("Content-Type", "multipart/form-data"), ("accept", "application/json")]
        body = [partBS "payload_json" "{}", partFileSource "file1" clipPath]
    manager <- getGlobalManager
    (response, status) <- postFormData manager webhookUrl headers body
    print status
    print response
    return ()


