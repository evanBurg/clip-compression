{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE NamedFieldPuns #-}
module Handlers where

import Foundation
import Yesod.Core
import Yesod.Core.Types (JSONResponse)
import Yesod.Form
import Data.Text
import GHC.Generics
import Data.Aeson
import Compress (compressVideo)
import SendMessage (sendMessage)

data CompressVideoResponse = CompressVideoResponse {
    success :: Bool
} deriving (Generic, Show)

instance FromJSON CompressVideoResponse
instance ToJSON CompressVideoResponse

uploadDirectory :: FilePath
uploadDirectory = "clips/"

putCompressR :: Handler Value
putCompressR = do
    App { webhookUrl } <- getYesod
    uploadedFile <- runInputPost $ iopt fileField "clipFile"
    case uploadedFile of 
        Just file -> do
            let filename = unpack $ fileName file
                destPath = uploadDirectory <> filename
            liftIO $ fileMove file destPath
            (scc, mPath) <- liftIO $ compressVideo filename
            case mPath of
                Just path -> liftIO $ sendMessage webhookUrl path
                Nothing -> liftIO $ print "No clip path"
            returnJson CompressVideoResponse {
                success = scc
            }
        Nothing -> returnJson CompressVideoResponse {
                success = False
            }

