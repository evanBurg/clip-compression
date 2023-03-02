{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Foundation where

import Yesod.Core
import Data.Word (Word64)
import GHC.Generics
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import Yesod.Form

data App = App {
    webhookUrl :: String,
    port :: Int
} deriving (Show, Generic, ToJSON, FromJSON)

data CompressVideoPayload = CompressVideoPayload {
    title :: String,
    description :: String,
    tags :: String
} deriving (Generic, Show, FromJSON, ToJSON)

newtype CompressVideoResponse = CompressVideoResponse {
    success :: Bool
} deriving (Generic, Show, FromJSON, ToJSON)

mkYesodData "App" $(parseRoutesFile "routes.yesodroutes")

instance Yesod App where
    maximumContentLength _ _ = Just $ 60 * 1024 * 1024 -- 60 megabytes

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage