{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use when" #-}
module Compress where

import Foundation
import Yesod.Core
import System.Exit
import System.Directory
import System.Process qualified as Proc
import System.IO (Handle)
import Data.String.Utils

-- 8mb = 64000Kib
-- 7mb = 56000Kib
desiredSize :: Double
desiredSize = 56000

-- bitrate = size (kib) / duration (seconds)
calcBitrate :: Double -> Int
calcBitrate duration = round $ desiredSize / duration

-- ffprobe -i input.mp4 -show_entries format=duration -v quiet -of csv="p=0"
-- ffmpeg -y -i input.mp4 -c:v libx264 -b:v 2600k -pass 1 -an -f null /dev/null && \
-- ffmpeg -i input.mp4 -c:v libx264 -b:v 2600k -pass 2 -c:a aac -b:a 128k output.mp4
compressVideo :: String -> IO (Bool, Maybe String)
compressVideo clipName = do
  currentPath <- getCurrentDirectory
  let clipPath = currentPath <> "/clips/" <> clipName
  let oClipPath = currentPath <> "/clips/compressed-" <> clipName
  clipExists <- doesFileExist clipPath
  case clipExists of 
    True -> do
        (exitCode, durationString, _) <- Proc.readCreateProcessWithExitCode (Proc.shell ("ffprobe -i " <> clipPath <> " -show_entries format=duration -v quiet -of csv=\"p=0\"")) ""
        print (strip durationString)
        let durationFloat = read (strip durationString) :: Double
            bitrate = calcBitrate durationFloat
        print $ show bitrate
        (exitCode, _, _) <- Proc.readCreateProcessWithExitCode (Proc.shell ("ffmpeg -y -i " <> clipPath <> " -c:v libx264 -b:v " <> show bitrate <> "k -pass 1 -an -f null /dev/null")) ""
        if exitCode == ExitSuccess then do
           (exitCode2, _, _) <- Proc.readCreateProcessWithExitCode (Proc.shell ("ffmpeg -i " <> clipPath <> " -c:v libx264 -b:v " <> show bitrate <> "k -pass 2 -c:a aac -b:a 128k " <> oClipPath)) ""
           return (exitCode == ExitSuccess, Just oClipPath)
        else return (False, Nothing)
    False -> do
        print $ "This file does not exist: " <> clipPath
        return (False, Nothing)