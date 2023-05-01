{- |
 Module: Domain.YoutubeFeed
 Description: Parses a Youtube feed into a domain model
-}
module Domain.YoutubeFeed (parse)
where

import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.RFC3339
import Domain.AudioFeed (AudioFeed (..))
import Domain.AudioFeed hiding (AudioFeed)
import Domain.AudioFeed qualified as Dom
import Domain.AudioFeed.Item as Dom
import Text.XML.Light

-- TODO use `xml-conduit` instead of using another XML library;
-- I couldn't figure out how to parse different tags inside `feed` :(
-- however, since the youtube feed contains only at most 15 items, streaming
-- isn't that necessary

parse :: Text -> Maybe AudioFeed
parse = parseAtom . parseXML

parseAtom :: [Content] -> Maybe Dom.AudioFeed
parseAtom contents = do
  let elements = onlyElems contents
  feed <- find ((== atomQual "feed") . elName) elements

  afTitle <- T.pack . strContent <$> findChild (atomQual "title") feed
  afLink <- findAlternateHref feed

  let entries = findElements (atomQual "entry") feed
  items <- traverse parseAtomEntry entries

  pure
    Dom.AudioFeed
      { afTitle
      , afLink
      , afItems = items
      }

-- | Finds the `href` of the "alternate" child `link` if any.
findAlternateHref :: Element -> Maybe Text
findAlternateHref element = do
  link <- filterChild (\e -> all ($ e) [hasAlternateRel, isLink]) element
  href <- findAttr (unqual "href") link
  pure $ T.pack href
  where
    isLink :: Element -> Bool
    isLink = (== atomQual "link") . elName

    hasAlternateRel :: Element -> Bool
    hasAlternateRel element' = case findAttr (unqual "rel") element' of
      Just "alternate" -> True
      _ -> False

parseAtomEntry :: Element -> Maybe Dom.AudioFeedItem
parseAtomEntry element = do
  videoId <-
    Dom.YoutubeVideoId . T.pack . strContent
      <$> findChild (qual "http://www.youtube.com/xml/schemas/2015" "videoId") element
  afiTitle <- T.pack . strContent <$> findChild (atomQual "title") element
  afiPubDate <- parsePubDate . strContent =<< findChild (atomQual "published") element
  afiDescription <- do
    group <- findChild (mediaQual "group") element
    T.pack . strContent <$> findChild (mediaQual "description") group
  afiLink <- findAlternateHref element

  pure
    Dom.AudioFeedItem
      { afiTitle
      , afiGuid = videoId
      , afiPubDate
      , afiDescription
      , afiLink
      }

parsePubDate :: String -> Maybe UTCTime
parsePubDate = fmap zonedTimeToUTC . parseTimeRFC3339

atomQual :: String -> QName
atomQual = qual "http://www.w3.org/2005/Atom"

mediaQual :: String -> QName
mediaQual = qual "http://search.yahoo.com/mrss/"

qual :: String -> String -> QName
qual uri name = QName{qName = name, qURI = Just uri, qPrefix = Nothing}
