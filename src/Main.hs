{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import qualified Control.Foldl as Fold
import qualified Data.Text as Text
import qualified Data.Text.Read as TR
import qualified Scenario
import Turtle

main :: IO ()
main = do
  downloadSeleniumServer
  setupChromedriver
  withSeleniumServer Scenario.run

downloadSeleniumServer :: IO ()
downloadSeleniumServer = do
  exists <- testfile (fromText seleniumServerJar)
  unless exists $ do
    echo "Downloading selenium server"
    procs "wget" ["https://selenium-release.storage.googleapis.com/3.141/selenium-server-standalone-3.141.59.jar"] empty

seleniumServerJar :: Text
seleniumServerJar = "selenium-server-standalone-3.141.59.jar"

withSeleniumServer :: IO () -> IO ()
withSeleniumServer action = do
  bracket
    (forkIO runSeleniumServer)
    killThread
    ( const $ do
        sleep 2
        action
        sleep 1
    )
  where
    runSeleniumServer :: IO ()
    runSeleniumServer =
      procs "java" ["-jar", "selenium-server-standalone-3.141.59.jar"] empty

setupChromedriver :: IO ()
setupChromedriver = do
  res1 <- getGoogleChromeVersionMajor
  case res1 of
    Version browserVersion -> do
      res2 <- getChromedriverVersionMajor
      case res2 of
        Version chromedriverVersion ->
          when (browserVersion /= chromedriverVersion) $
            downloadChromedriver browserVersion
        NotAvailable -> downloadChromedriver browserVersion
        _ -> die $ Text.pack $ show res2
    _ -> die $ Text.pack $ show res1

data CommandResult
  = NotAvailable
  | VersionParseFailure Text
  | NoOutput
  | Version Int
  deriving (Show)

{- "Google Chrome 79.0.3945.130" -> Right 79-}
getGoogleChromeVersionMajor :: IO CommandResult
getGoogleChromeVersionMajor = do
  mChromeBinary <- which "google-chrome"
  case mChromeBinary of
    Nothing -> pure NotAvailable
    Just _chromeBinary -> do
      mStdout <- fold (inshell "google-chrome --version" empty) Fold.head
      case fmap lineToText mStdout of
        Nothing -> pure NoOutput
        Just out ->
          case Text.words out of
            (_ : _ : versionText : _) ->
              case TR.decimal versionText of
                Left er -> pure $ VersionParseFailure $ Text.pack er
                Right (numericVersion, _) -> pure $ Version numericVersion
            _ -> pure $ VersionParseFailure out

getChromedriverVersionMajor :: IO CommandResult
getChromedriverVersionMajor = do
  exists <- testfile "chromedriver"
  if exists
    then do
      mStdout <- fold (inshell "./chromedriver --version" empty) Fold.head
      case fmap lineToText mStdout of
        Nothing -> pure NoOutput
        Just out ->
          case Text.words out of
            (_ : versionText : _) ->
              case TR.decimal versionText of
                Left er -> pure $ VersionParseFailure $ Text.pack er
                Right (numericVersion, _) -> pure $ Version numericVersion
            _ -> pure $ VersionParseFailure out
    else pure NotAvailable

downloadChromedriver :: Int -> IO ()
downloadChromedriver numericVersion = do
  let releaseUrl =
        Text.pack $
          "https://chromedriver.storage.googleapis.com/LATEST_RELEASE_"
            <> show numericVersion
  mExactVersion <- fold (inproc "curl" [releaseUrl] empty) Fold.head
  case mExactVersion of
    Nothing ->
      die $
        "Failed to retrieve exact version of chromedriver from " <> releaseUrl
    Just exactVersion -> do
      let chromedriverZip = "chromedriver_linux64.zip"
          chromedriverUrl =
            "https://chromedriver.storage.googleapis.com/"
              <> lineToText exactVersion
              <> "/"
              <> chromedriverZip
      sh $ do
        procs "wget" [chromedriverUrl] empty
        procs "unzip" [chromedriverZip] empty
        rm $ fromText chromedriverZip
