module App.Rest (rest, randomRestEvent) where

import App.Types
import Prelude

import App.Game (withChance)
import App.Player (Player(..), RandomEvent, limit, spendTime, update, withdraw)
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM, randomRange)
import Data.Either (Either(..), note)
import Data.List.Lazy (List(..), Step(..), dropWhile, filterM, fromFoldable, head)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, tuple3)

go :: Activity -> Player -> Player
go food = case food of
  Nap -> update m 0
  SurfInternet -> update m (-10)
  GetDrunk -> update m (-30)
  where m = mood food

rest :: Player -> Activity -> Either String Player
rest pl rest = do
    pl <- note "Не на что куралесить" $ withdraw (price rest) pl
    pl <- note "Некогда епты" $ spendTime (time rest) pl
    pure $ go rest pl


type RestEvent = {chance :: Number, msg :: String, mood :: Mood}

restEvents :: Activity -> List RestEvent
restEvents activity = fromFoldable case activity of
  Nap ->
    [ { chance: 5.0, msg: "Приснилась баба срака", mood: -10 }
    , { chance: 5.0, msg: "Приснился няшный трап", mood: 10 }
    , { chance: 5.0, msg: "Заболела голова", mood: -10 }
    , { chance: 5.0, msg: "Хорошо прикорнул", mood: 10 }
    ]
  SurfInternet ->
    [ { chance: 10.0, msg: "Затралили", mood: -30 }
    , { chance: 10.0, msg: "Даркоблядь опять выиграет", mood: -30 }
    , { chance: 10.0, msg: "Шебм с убийствами котиков :с", mood: -30 }
    , { chance: 10.0, msg: "Посмеялся-Обосрался", mood: 20 }
    , { chance: 10.0, msg: "Обдвачевался", mood: 20 }
    , { chance: 10.0, msg: "Затралил лалок", mood: 20 }
    , { chance: 10.0, msg: "Серанул от скримера", mood: -10 }
    , { chance: 10.0, msg: "Получил 2 чая", mood: 10 }
    ]
  GetDrunk ->
    [ { chance: 10.0, msg: "Поймал белку", mood: -40 } ]

randomRestEvent :: forall eff. Activity -> Aff (random :: RANDOM | eff) (Maybe RandomEvent)
randomRestEvent activity = do
  list <- filterM select $ restEvents activity
  pure $ go <$> head list
  where
    go :: RestEvent -> RandomEvent
    go { msg, mood } = \pl -> Tuple (msg <> " " <> showMood mood) $ update mood 0 pl

    select {chance} = do
      r <- liftEff $ randomRange 0.0 100.0
      pure $ r < chance
