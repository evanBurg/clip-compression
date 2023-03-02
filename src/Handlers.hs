{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
import Data.Map qualified as Map

uploadDirectory :: FilePath
uploadDirectory = "clips/"

postCompressR :: Handler Value
postCompressR = do
    App { webhookUrl } <- getYesod
    -- payload <- requireCheckJsonBody :: Handler CompressVideoPayload
    -- title <- lookupPostParam "title"
    -- desc <- lookupPostParam "description"
    -- tags <- lookupPostParam "tags"
    let payload = CompressVideoPayload {
        title = Just "Test title"
        , description = Just "Test description"
        , tags = Just ["one", "two", "three"]
    }
    uploadedFile <- runInputPost $ iopt fileField "clipFile"
    -- returnJson payload
    case uploadedFile of 
        Just file -> do
            liftIO $ print "HERE"
            let filename = unpack $ fileName file
                destPath = uploadDirectory <> filename
            liftIO $ print filename
            liftIO $ print destPath
            liftIO $ fileMove file destPath
            (scc, mPath) <- liftIO $ compressVideo filename
            case mPath of
                Just path -> liftIO $ sendMessage payload webhookUrl path
                Nothing -> liftIO $ print "No clip path"
            returnJson CompressVideoResponse {
                success = scc
            }
        Nothing -> returnJson CompressVideoResponse {
                success = False
            }
