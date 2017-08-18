module App.Food (eat, randomFoodEvent, satiety) where

import App.Types
import Prelude

import App.Game (withChance)
import App.Player (Player(..), Player, RandomEvent, fullnessPerHour, hasTime, limit, spendTime, update, updateWithTime, withdraw)
import App.Types (class Hint, Food(..))
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

satiety :: Food -> Int
satiety SemiFinished = 25
satiety NormalFood = 40
satiety Cafe = 60

consume :: Food -> Player -> Player
consume food = update (mood food) fullness where
  fullness = satiety food + time food * fullnessPerHour

eat :: Player -> Food -> Either String Player
eat {fullness} _ | fullness >= limit = Left "Не хочется"
eat pl food = do
    pl <- note "Жрать не на что" $ withdraw (price food) pl
    pl <- note "Все магазины и кафе закрыты" $ spendTime (time food) pl
    pure $ consume food pl


doShit :: RandomEvent
doShit pl
  | pl `hasTime` 1 = Tuple "Просрался: -1ч. ☹10  -20☕" $ updateWithTime (-10) ((-20) + fullnessPerHour) 1 pl
  | otherwise = Tuple "Дристали всю ночь: ☹10  -20☕" $ updateWithTime (-10) ((-20) + fullnessPerHour) 1 pl

randomFoodEvent :: forall eff. Food -> Aff (random :: RANDOM | eff) (Maybe RandomEvent)
randomFoodEvent food = withChance c $ doShit where
  c = case food of
    SemiFinished -> 20.0
    NormalFood -> 5.0
    Cafe -> 5.0
