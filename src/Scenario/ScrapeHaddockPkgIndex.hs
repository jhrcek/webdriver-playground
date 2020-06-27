{-# LANGUAGE OverloadedStrings #-}

module Scenario.ScrapeHaddockPkgIndex (run) where

import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as Text
import Data.Text (Text)
import Test.WebDriver

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

run :: IO ()
run = do
  runSession chromeConfig . finallyClose $ do
    openPage "https://hackage.haskell.org/package/Cabal-3.2.0.0/docs/doc-index-All.html"
    indexTableBody <- findElem (ByCSS "#index table tbody")
    maybeBody <- attr indexTableBody "innerText"
    for_ maybeBody $ \tbody -> do
      let indexItems = parseIndexItems tbody
      let reverseIndex = buildReverseIndex indexItems
      liftIO
        $ traverse_ print
        $ List.sortOn snd
        $ Map.toList
        $ fmap length reverseIndex

-- | Index item corresponds to one or more lines in package's haddock index
data IndexItem
  = IndexItem Symbol [Text]
  deriving (Show)

data Symbol
  = -- | Thing exposed from one place
    Simple Text
  | -- | Ambiguous symbol declared in mulptiple modules. `Ambiguous "1 (Type/Class)" "AbiDependency" `
    Ambiguous Text Text
  deriving (Show)

parseIndexItems :: Text -> [IndexItem]
parseIndexItems indexText = chomp rows
  where
    rows = fmap (Text.splitOn "\t") $ Text.lines indexText
    chomp ([symbol, "\160"] : rest) =
      let (meanings, rest1) = span (Text.isInfixOf " (" . head) rest
       in [IndexItem (Ambiguous meaning symbol) (Text.splitOn ", " commaSepModules) | [meaning, commaSepModules] <- meanings] ++ chomp rest1
    chomp ([symbol, commaSepModules] : rest) = IndexItem (Simple symbol) (Text.splitOn ", " commaSepModules) : chomp rest
    chomp [] = []
    chomp x = error $ "Was expecting to have two TAB separated items on line: " <> show x

buildReverseIndex :: [IndexItem] -> Map Text [Symbol]
buildReverseIndex items =
  foldr
    ( \(IndexItem sym modules) modToSymbols ->
        foldr
          (\mdl -> Map.insertWith (++) mdl [sym])
          modToSymbols
          modules
    )
    Map.empty
    items
