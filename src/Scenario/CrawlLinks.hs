{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Scenario.CrawlLinks where

import Control.Monad.IO.Class
import qualified Data.List as List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import Test.WebDriver
import Test.WebDriver.Class (WebDriver)
import Text.HTML.TagSoup

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

run :: String -> IO ()
run startUrl = do
  runSession chromeConfig . finallyClose $ do
    crawledState <- crawl (initState $ Text.pack startUrl)
    liftIO $ print crawledState

initState :: Text -> CrawlState
initState url = CrawlState {visited = Set.empty, toVisit = [url], links = []}

data CrawlState = CrawlState
  { visited :: Set Text,
    toVisit :: [Text],
    links :: [(Text, Text)]
  }
  deriving (Show)

crawl :: CrawlState -> WD CrawlState
crawl cs@CrawlState {visited, toVisit, links} =
  case toVisit of
    [] -> pure cs
    (url : urls) ->
      if Set.member url visited
        then
          crawl $
            CrawlState
              { visited = visited,
                toVisit = urls,
                links = links
              }
        else do
          liftIO $ putStrLn $ "New page " <> Text.unpack url
          openPage $ Text.unpack $ localPrefix <> url
          newLinks <- getLocalLinks
          crawl $
            CrawlState
              { visited = Set.insert url visited,
                toVisit = newLinks <> urls,
                links = fmap (url,) newLinks <> links
              }

getLocalLinks :: (MonadIO wd, WebDriver wd) => wd [Text]
getLocalLinks = do
  sourceText <- getSource
  let hrefs = mapMaybe getHref $ filter (isTagOpenName "a") $ parseTags sourceText
  liftIO $ print hrefs
  pure $ List.nub $ fmap stripHash $
    filter
      (\url -> not (Text.isInfixOf "http" url) && not (Text.isInfixOf "jhrcek" url))
      {-mapMaybe (Text.stripPrefix localPrefix) -} hrefs

getHref :: Tag Text -> Maybe Text
getHref tag = case fromAttrib "href" tag of
  "" -> Nothing
  x -> Just x

stripHash :: Text -> Text
stripHash url = Text.takeWhile (/= '#') url

localPrefix :: Text
localPrefix = "file:///home/jhrcek/Devel/github.com/haskell-servant/servant/.stack-work/install/x86_64-linux-tinfo6/dd6fe7f35c08d19765cf27f199e3aebfab7765b3e84e9b2e40651df5c3594a59/8.6.5/doc/servant-0.17/"
