{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import Yesod.Core
import Data.Word (Word64)

data App = App

mkYesodData "App" $(parseRoutesFile "routes.yesodroutes")

instance Yesod App where
    maximumContentLength _ _ = Just $ 40 * 1024 * 1024 -- 40 megabytes
