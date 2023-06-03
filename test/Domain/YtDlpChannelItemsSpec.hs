{-# LANGUAGE QuasiQuotes #-}

module Domain.YtDlpChannelItemsSpec (spec) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy qualified as LBS (ByteString)
import Data.Text.Lazy.Encoding
import Domain.AudioFeed.Item
import Domain.YoutubeVideoId
import Domain.YtDlpChannelItems
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses channel item" $ do
      (eitherDecode' itemBytes >>= parseEither parse) `shouldBe` Right item

{- | One line of sample output from
`yt-dlp -f ba -O '%(.{id,title,description,webpage_url,upload_date})+j' --no-warnings --ignore-no-formats-error --no-check-formats https://www.youtube.com/channel/id`
-}
itemBytes :: LBS.ByteString
itemBytes =
  encodeUtf8
    [r|{"id": "7NCf-uanfa4", "title": "#57 Книга Исход (Exodus) Главa 15", "description": "Программист Яков Файн читает и комментирует 15-ю главу  Книги Исход, гдe израильтяне пели, а женщины даже и танцевали. А затем продолжили движение в сторону Горы Синай\n\nИсходник здесь: https://bible.by/nrt/2/15\n \nМои Youtube каналы: \n\n1. Yakov Fain: https://www.youtube.com/channel/UCnExw5tVdA3TJeb4kmCd-JQ \n2. Карьера в ИТ:  https://www.youtube.com/channel/UCdI5EH48Quo0Cope7Jvg-Pg \n3. Программист читает библию: https://www.youtube.com/channel/UCB4-nBNOP0N1J2Sh-A51sFQ", "webpage_url": "https://www.youtube.com/watch?v=7NCf-uanfa4", "upload_date": "20230516"}|]

item :: AudioFeedItem
item =
  AudioFeedItem
    { afiGuid = YoutubeVideoId "7NCf-uanfa4"
    , afiTitle = "#57 Книга Исход (Exodus) Главa 15"
    , afiPubDate = read "2023-05-16 00:00:00 UTC"
    , afiDescription =
        "Программист Яков Файн читает и комментирует 15-ю главу  Книги Исход, гдe израильтяне пели, а женщины даже и танцевали. А затем продолжили движение в сторону Горы Синай\n\nИсходник здесь: https://bible.by/nrt/2/15\n \nМои Youtube каналы: \n\n1. Yakov Fain: https://www.youtube.com/channel/UCnExw5tVdA3TJeb4kmCd-JQ \n2. Карьера в ИТ:  https://www.youtube.com/channel/UCdI5EH48Quo0Cope7Jvg-Pg \n3. Программист читает библию: https://www.youtube.com/channel/UCB4-nBNOP0N1J2Sh-A51sFQ"
    , afiLink = "https://www.youtube.com/watch?v=7NCf-uanfa4"
    }
