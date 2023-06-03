module Usecases.RssConduitExt
  ( renderRssDocumentStreaming
  ) where

import Conduit
import Control.Monad
import Data.Text as Text
import Data.Text.Encoding
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.RFC822
import Data.Version
import Data.XML.Types
import Lens.Micro
import Text.RSS.Conduit.Render
import Text.RSS.Extensions
import Text.RSS.Lens
import Text.RSS.Types
import Text.XML.Stream.Render
import URI.ByteString

-- TODO submit a PR upstream

{- | Render the top-level @\<rss\>@ element, reading additional @RssItem@s from
the conduit — they are rendered after @channelItems@.
-}
renderRssDocumentStreaming
  :: (Monad m, RenderRssExtension e) => RssDocument e -> ConduitT (RssItem e) Event m ()
renderRssDocumentStreaming d = tag "rss" (attr "version" . pack . showVersion $ d ^. documentVersionL) $
  tag "channel" mempty $ do
    mapToEmptyInput $ do
      textTag "title" $ d ^. channelTitleL
      textTag "link" $ renderRssURI $ d ^. channelLinkL
      textTag "description" $ d ^. channelDescriptionL
      optionalTextTag "copyright" $ d ^. channelCopyrightL
      optionalTextTag "language" $ d ^. channelLanguageL
      optionalTextTag "managingEditor" $ d ^. channelManagingEditorL
      optionalTextTag "webMaster" $ d ^. channelWebmasterL
      forM_ (d ^. channelPubDateL) $ dateTag "pubDate"
      forM_ (d ^. channelLastBuildDateL) $ dateTag "lastBuildDate"
      forM_ (d ^. channelCategoriesL) renderRssCategory
      optionalTextTag "generator" $ d ^. channelGeneratorL
      forM_ (d ^. channelDocsL) $ textTag "docs" . renderRssURI
      forM_ (d ^. channelCloudL) renderRssCloud
      forM_ (d ^. channelTtlL) $ textTag "ttl" . tshow
      forM_ (d ^. channelImageL) renderRssImage
      optionalTextTag "rating" $ d ^. channelRatingL
      forM_ (d ^. channelTextInputL) renderRssTextInput
      renderRssSkipHours $ d ^. channelSkipHoursL
      renderRssSkipDays $ d ^. channelSkipDaysL
      forM_ (d ^. channelItemsL) renderRssItem
    awaitForever $ \item -> mapToEmptyInput (renderRssItem item)
    mapToEmptyInput $ renderRssChannelExtension $ d ^. channelExtensionsL

mapToEmptyInput :: (Monad m) => ConduitT () Event m () -> ConduitT i Event m ()
mapToEmptyInput = mapInput (const ()) (const Nothing)

-- Utils copied from `Text.RSS.Conduit.Render`

tshow :: (Show a) => a -> Text
tshow = pack . show

textTag :: (Monad m) => Name -> Text -> ConduitT () Event m ()
textTag name = tag name mempty . content

optionalTextTag :: (Monad m) => Name -> Text -> ConduitT () Event m ()
optionalTextTag name value = unless (Text.null value) $ textTag name value

dateTag :: (Monad m) => Name -> UTCTime -> ConduitT () Event m ()
dateTag name = tag name mempty . content . formatTimeRFC822 . utcToZonedTime utc

renderRssURI :: RssURI -> Text
renderRssURI = decodeUtf8 . withRssURI serializeURIRef'
