{-# LANGUAGE OverloadedStrings #-}

module Scenario where

import           Test.WebDriver

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome (defaultConfig {wdHost = "192.168.122.1"})

run :: IO ()
run =
  runSession chromeConfig . finallyClose $ do
    openPage "http://google.com"
    searchInput <- findElem (ByCSS "input[type='text']")
    sendKeys "Hello, World!" searchInput
    submit searchInput
