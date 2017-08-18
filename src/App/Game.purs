module App.Game where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM, randomInt, randomRange)
import Data.Array (elem, length, (!!))
import Data.DateTime (DateTime, adjust)
import Data.JSDate (jsdate, toDateTime)
import Data.Maybe (Maybe, Maybe(..), fromJust)
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
{-
короче загатовка =
1)движок = надо учить юнити
2) пиксельарт = надо рисовать пиксельарт
3) звук = надо тратить 3 часа в день чтобы искать музыку в интернете
4) диздок = думать над сценарием 4 часа в день, теряя настроение
-}


endDate :: DateTime
endDate = unsafePartial $ fromJust $ toDateTime $
          jsdate { year: 2017.0
                 , month: 7.0
                 , day: 17.0
                 , hour: 0.0
                 , minute: 0.0
                 , second: 0.0
                 , millisecond: 0.0
                 }

startDate :: DateTime
startDate = unsafePartial $ fromJust $ adjust (Days (-90.0)) endDate


randomSearchQuery :: forall eff. Eff (random :: RANDOM | eff) String
randomSearchQuery = do
  i <- randomInt 0 $ (length queries - 1)
  pure $ unsafePartial $ fromJust $ queries !! i
  where
    queries = [ "аниме"
              , "онемэ"
              , "абу"
              , "бабу"
              , "half-life 3"
              , "/gd"
              , "/bb"
              , "фото 2007"
              , "купить ассеты"
              , "скачать ассеты"
              , "кэ?"
              ]


withChance :: forall a eff. Number -> a -> Aff (random :: RANDOM | eff) (Maybe a)
withChance c res = do
  r <- liftEff $ randomRange 0.0 100.0
  pure if r <= c
       then Just res
       else Nothing
