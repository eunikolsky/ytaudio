{-# LANGUAGE QuasiQuotes #-}

module Domain.YoutubeFeedSpec (spec) where

import Data.Text (Text)
import Data.Text.IO qualified as T
import Domain.AudioFeed hiding (AudioFeed)
import Domain.AudioFeed qualified as Dom
import Domain.AudioFeed.Item hiding (AudioFeedItem)
import Domain.AudioFeed.Item qualified as Dom
import Domain.YoutubeFeed (parse)
import Paths_ytaudio (getDataFileName)
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses a valid feed" $ do
      validFeedText <- getValidFeedText
      parse validFeedText `shouldBe` Just feed

getValidFeedText :: IO Text
getValidFeedText = do
  {-
   - github copilot suggested this `getDataFileName` function, which comes from
   - `Paths_X` generated by `cabal` and I didn't know about. See more details at
   - <https://cabal.readthedocs.io/en/stable/cabal-package.html#accessing-data-files-from-package-code>.
   -
   - this is copilot's first important suggestion
   -}
  validFeedPath <- getDataFileName "test/Domain/videos.xml"
  T.readFile validFeedPath

feed :: Dom.AudioFeed
feed =
  Dom.AudioFeed
    { afTitle = "Yakov Fain"
    , afLink = "https://www.youtube.com/channel/UCnExw5tVdA3TJeb4kmCd-JQ"
    , afItems =
        [ Dom.AudioFeedItem
            { afiTitle = "#1283 Переговоры начнутся осенью"
            , afiGuid = YoutubeVideoId "Cl3aOUbGVbE"
            , afiPubDate = read "2023-04-30 02:21:51 UTC"
            , afiDescription =
                "00:00 Комментарии\n06:49 Еврей из США не может рассуждать\n09:50 Пять острых проблем Украины \n14:17 Комментарии\n…"
            , afiLink = "https://www.youtube.com/watch?v=Cl3aOUbGVbE"
            }
        , Dom.AudioFeedItem
            { afiTitle = "#1282 Русский народ бомбит спящих жителей Умани"
            , afiGuid = YoutubeVideoId "o2lki2W8nGA"
            , afiPubDate = read "2023-04-29 02:21:58 UTC"
            , afiDescription =
                [r|00:00 Комментарии
02:24 На концерте группы Океан Эльзы
07:43 Комментарии
09:18 Русский народ бомбит спящих граждан в Умани
15:05 Комментарии
15:25 Рекоммендации Зеленского по военкомам
19:55 Одесский военком жирует
23:00 Комментарии
25:07 Украинцы на Уимблдоне?
27:55 Комментарии
28:28 Все товары под замок
31:23 Комментарии
32:48 ДеСантис против Диснея
48:45 Загорелся самолет

Мои youtube каналы

1. Yakov Fain: https://www.youtube.com/channel/UCnExw5tVdA3TJeb4kmCd-JQ 
2. Карьера в ИТ:  https://www.youtube.com/channel/UCdI5EH48Quo0Cope7Jvg-Pg 
3. Программист читает библию: https://www.youtube.com/channel/UCB4-nBNOP0N1J2Sh-A51sFQ|]
            , afiLink = "https://www.youtube.com/watch?v=o2lki2W8nGA"
            }
        ]
    }