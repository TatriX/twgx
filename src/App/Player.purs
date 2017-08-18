module App.Player where

import App.Types
import Prelude

import App.Game (endDate, startDate)
import Data.DateTime (DateTime(..), adjust, diff)
import Data.Either (Either(..))
import Data.Int (ceil, fromNumber, toNumber)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing)
import Data.Time.Duration (Days(..), convertDuration)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

defaultFreeTime :: FreeTime
defaultFreeTime = {left: 16, max: 16}

type Player =  { name :: String
               , fullness :: Fullness
               , money :: Money
               , mood :: Mood
               , job :: Job
               , project :: Project
               , freetime :: {left :: Int, max :: Int}
               , date :: DateTime
               , mommyHelped :: DateTime
               , salaryDate :: Maybe DateTime
               }

newPlayer :: String -> Player
newPlayer name = { name: name
                 , fullness: limit / 2
                 , money: 20.0
                 , mood: limit / 2
                 , project: emptyProject
                 , job: Jobless
                 , freetime: defaultFreeTime
                 , date: startDate
                 , mommyHelped: addDays (-7) startDate
                 , salaryDate: Nothing
                 }


type RandomEvent = Player -> (Tuple String Player)

limit :: Int
limit = 100

hasMoney :: Player -> Money -> Boolean
hasMoney pl money = pl.money >= money

withdraw :: Money -> Player -> Maybe Player
withdraw money pl
  | pl `hasMoney` money = Just $ pl { money = pl.money - money }
  | otherwise = Nothing

fullnessPerHour :: Int
fullnessPerHour = 5

daysToSalary :: Player -> Maybe Int
daysToSalary pl = case pl.job of
  Jobless -> Nothing
  _ -> case pl.salaryDate of
      Nothing -> Nothing
      Just date -> let (Days n) = (diff date pl.date) in Just $ ceil n

nextSalaryDate :: Job -> DateTime -> Maybe DateTime
nextSalaryDate Jobless _ = Nothing
nextSalaryDate job date =
  adjust delta date where
    (Salary _ period) = salary job

    delta = case period of
      Day -> Days 1.0
      Week -> Days 8.0
      Month -> Days 30.0

updateSalary :: Player -> DateTime -> Money
updateSalary { salaryDate, money, job } date
  | salaryDate == Just date = money + let (Salary n _) = salary job in money + n
  | otherwise = money

updateSalaryDate :: Player -> DateTime -> Maybe DateTime
updateSalaryDate { salaryDate, job } date
  | salaryDate == Just date = nextSalaryDate job date
  | otherwise = salaryDate

spendTime :: Time -> Player -> Maybe Player
spendTime time pl
  | pl `hasTime` time = Just $ pl { freetime { left = left' }
                                  , date = date
                                  , money = money
                                  , fullness = fullness
                                  , salaryDate = salaryDate
                                  } where
    fullness = max 0 $ pl.fullness - fullnessPerHour *  time
    left = pl.freetime.left - time
    nextDay = left == 0
    date = if nextDay then nextDate pl.date else pl.date
    salaryDate = if nextDay then updateSalaryDate pl date else pl.salaryDate
    money = if nextDay then updateSalary pl date else pl.money
    left' = case left of
      0 -> pl.freetime.max
      t -> t
  | otherwise = Nothing

addDays :: Int -> DateTime -> DateTime
addDays days date = unsafePartial $ fromJust $ adjust (Days $ toNumber days) date

nextDate :: DateTime -> DateTime
nextDate date = addDays 1 date

hasTime :: Player -> Int -> Boolean
hasTime pl t = pl.freetime.left >= t

update :: Mood -> Fullness -> Player -> Player
update mood fullness pl =
  pl { mood = max 0 $ min limit $ pl.mood + mood
     , fullness = max 0 $ min limit $ pl.fullness + fullness
     }

updateWithTime :: Mood -> Fullness -> Time -> Player -> Player
updateWithTime mood fullness time pl = update mood fullness pl' where
  pl' = if pl `hasTime` time
        then unsafePartial $ fromJust $ spendTime time pl
        else skipDay pl


outOfTime :: Player -> Boolean
outOfTime {date} = days <= 0.0 where
  (Days days) = diff endDate date

dead :: Player -> Maybe String
dead pl@{fullness, mood, money}
  | fullness <= 0 = Just "Ты конечно дрыщ, но все равно так низя"
  | mood <= 0 = Just "От такой жизни ты вскрыл себе ебало"
  | money <= 0.0 = Just "Ребята коллекторы посадили тебя на бутылку"
  | outOfTime pl = Just "Ты просрал все полимеры"
  | otherwise = Nothing

revive :: Player -> Player
revive pl@{fullness, mood, money, date}
  | fullness <= 0 = pl { fullness = pl.fullness + 1}
  | mood <= 0 = pl { mood = pl.mood + 1}
  | money <= 0.0 = pl { money = pl.money + 1.0}
  | otherwise = pl

canAskMommy :: Player -> Boolean
canAskMommy pl = pl.date `diff` pl.mommyHelped >= Days 7.0

askMommy :: Player -> Number -> Either String Player
askMommy pl n
  | canAskMommy pl = Right $ pl { money = pl.money + n, mommyHelped = pl.date }
  | otherwise = Left "Сына, ты что фея шлюха?"


skipDay :: Player -> Player
skipDay pl = unsafePartial $ fromJust $ spendTime pl.freetime.left pl
