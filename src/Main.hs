{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Foldl  as Fold
import qualified Data.Text      as Text
import qualified Data.Text.Read as TR
import           Turtle

import qualified Scenario


main :: IO ()
main = do
  Scenario.run
  
setupChromedriver :: IO ()
setupChromedriver = do
  eMajorVersion <- getGoogleChromeVersionMajor
  case eMajorVersion of 
    Left err -> die err
    Right majorVersion -> downloadChromedriver majorVersion
    

{- "Google Chrome 79.0.3945.130" -> Right 79-}
getGoogleChromeVersionMajor :: IO (Either Text Int)
getGoogleChromeVersionMajor = do
  mChromeBinary <- which "google-chrome"
  case mChromeBinary of
    Nothing -> throw "`google-chrome` binary is not on PATH"
    Just chromeBinary -> do
      mStdout <- fold (inshell "google-chrome --version" empty) Fold.head
      case fmap lineToText mStdout of
        Nothing -> throw "`google-chrome --version` didn't produce output"
        Just out ->
          case Text.words out of
            (_:_:versionText:_) ->
              case TR.decimal versionText of
                Left err                  -> throw $ Text.pack err
                Right (numericVersion, _) -> pure $ Right numericVersion
            _ ->
              throw $ "`google-chrome --version` had unexpected output: " <> out
  where
    throw = pure . Left

downloadChromedriver :: Int -> IO ()
downloadChromedriver numericVersion = do
  let releaseUrl = Text.pack $ "https://chromedriver.storage.googleapis.com/LATEST_RELEASE_" <> show numericVersion
  mExactVersion <- fold (inproc "curl" [releaseUrl] empty) Fold.head
  case mExactVersion of
    Nothing -> die $ "Failed to retrieve exact version of chromedriver from " <> releaseUrl
    Just exactVersion -> do
      let chromedriverZip = "chromedriver_linux64.zip"
          chromedriverUrl = "https://chromedriver.storage.googleapis.com/" <> lineToText exactVersion <> "/" <> chromedriverZip
      sh $ do
        procs "wget" [chromedriverUrl] empty
        procs "unzip" [chromedriverZip] empty
        rm $ fromText chromedriverZip

-- TODO automate selenium server download https://selenium.dev/downloads/
-- TODO autoamte selenium server start/stop