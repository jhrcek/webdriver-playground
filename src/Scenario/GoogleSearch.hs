{-# LANGUAGE OverloadedStrings #-}

module Scenario.GoogleSearch where

import Test.WebDriver

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

run :: IO ()
run = do
  runSession chromeConfig . finallyClose $ do
    openPage "http://google.com"
    searchInput <- findElem (ByCSS "input[type='text']")
    sendKeys "Hello, World!" searchInput
    submit searchInput
