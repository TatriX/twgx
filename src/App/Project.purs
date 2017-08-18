module App.Project where

import Prelude

import App.Player (Player(..), limit, spendTime, update, withdraw)
import App.State (Achievement)
import App.Types (class Task, Engine(..), Graphics(..), Idea(..), Music(..), Project, ProjectTask, ProjectTask(..), ProjectWork(..), efforts, list, price, toProjectTask)
import Data.Array (dropWhile, head, last, length, tail, takeWhile)
import Data.Either (Either(..), either, note)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

totalProgress :: Project -> Int
totalProgress pr = (p pr.idea + p pr.engine + p pr.graphics + p pr.music) / 4 where
  p = currentProgress

workOnProject :: forall a. Task a => Player -> a -> Either String Player
workOnProject pl task = do
  pl <- note "Я не успею за сегодня. Инфа соточка." $ spendTime time pl
  pure $ update (-mood) 0 pl
  where {time, mood} = efforts task

buyAssets :: Player -> ProjectTask -> Either String Player
buyAssets pl task = do
  pl <- note "Нищеброд, штолле?" $ withdraw (price BuyAssets) pl
  pure $ case task of
    Idea -> pl { project { idea = Nothing } }
    Engine -> pl { project { engine = Nothing } }
    Graphics -> pl { project { graphics = Nothing } }
    Music -> pl { project { music = Nothing } }

next :: forall a. Task a => a -> Maybe a
next task = dropWhile (_ /= task) list # tail >>= head

prev :: forall a. Task a => a -> Maybe a
prev task = last $ takeWhile (_ /= task) list

currentProgress :: forall a. Task a => Maybe a -> Int
currentProgress Nothing = 100
currentProgress (Just x) = go 0 x where
  go acc x = case prev x of
    Nothing -> acc
    Just x' -> go (acc + (efforts x').progress) x'

finished :: Project -> Boolean
finished pr = totalProgress pr == 100

nextStep :: Player -> ProjectTask -> Player
nextStep pl task = case task of
  Idea -> pl { project { idea = next =<< pl.project.idea }}
  Engine -> pl { project { engine = next =<< pl.project.engine }}
  Graphics -> pl { project { graphics = next =<< pl.project.graphics }}
  Music -> pl { project { music = next =<< pl.project.music }}

achievement :: Player -> ProjectTask -> Maybe Achievement
achievement {project} task = case task of
  Idea -> case project.idea of
    Just Story -> Just { name: "Ахуенно, теперь можно думать дальше", file: "idea-1.png" }
    Just Enemies -> Just { name: "Я твой дом идея шатал", file: "idea-2.png" }
    _ -> Nothing
  Graphics -> case project.graphics of
    Just DownloadPhotoshop -> Just { name: "Отлично, теперь можно пробовать себя в Pixel Art!"
                                   , file: "graphics-1.png"
                                   }
    Just DrawEnemies -> Just { name: "Я у мамы хуйдожник"
                                   , file: "graphics-2.png"
                                   }
    _ -> Nothing
  Music -> case project.music of
    Just DoMusic -> Just { name: "I Like this music!", file: "music.png" }
    _ -> Nothing
  Engine -> case project.engine of
    Just DownloadUnity -> Just { name: "Если бы у тебя был мозг, он выглядел бы так", file: "engine-1.png" }
    Just DoAssets -> Just { name: "Мастер таскания ассетов", file: "engine-2.png" }
    _ -> Nothing


tryTask :: Player -> ProjectTask -> Int -> Tuple (Maybe String) Player
tryTask pl ptask chance = case ptask of
  Idea -> case work pl.project.idea of
    Left err -> Tuple (Just err) pl
    Right pl -> if chance < 50
                then Tuple Nothing $ nextStep pl ptask
                else Tuple (Just "Ты тупой, поэтому ничего не придумал.") pl
  Music -> case work pl.project.music of
    Left err -> Tuple (Just err) pl
    Right pl -> if chance < 25
                then Tuple Nothing $ nextStep pl ptask
                else Tuple (Just "Вышла какая-то хуйня.") pl
  Engine -> try pl.project.engine
  Graphics -> try pl.project.graphics
  where
    work :: forall a. Task a => Maybe a -> Either String Player
    work Nothing = Left "Разраб обосрался. Извините."
    work (Just task) = workOnProject pl task

    try :: forall a. Task a => Maybe a -> Tuple (Maybe String) Player
    try task = either
       (\err -> Tuple (Just err) pl)
       (\pl -> Tuple Nothing $ nextStep pl ptask)
       $ work task
