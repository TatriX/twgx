module App.Types where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomBool)
import Data.Array (elemIndex, filterA, index)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Partial.Unsafe (unsafePartial)

class Hint a where
  time :: a -> Time
  price :: a -> Money
  mood :: a -> Mood

type Money = Number
type Fullness = Int
type Mood = Int

type Time = Int

type FreeTime = {left :: Time, max :: Time}

class RandomList a where
  randomList :: forall eff. Eff (random :: RANDOM | eff) (Array a)

-- job
data Job = Jobless | Admin | Promoter | MuckDuck

instance showJob :: Show Job where
  show Jobless = "Безработный"
  show Admin = "Админ"
  show Promoter = "Раздача листовок"
  show MuckDuck = "Макдак"

data Period = Day | Week | Month

instance showPeriod :: Show Period where
  show Day = "день"
  show Week = "неделю"
  show Month = "месяц"

instance showSalary :: Show Salary where
  show (Salary m p) = show m <> "$ в " <> show p

data Salary = Salary Money Period

salary :: Job -> Salary
salary Jobless = Salary 0.0 Day
salary Admin = Salary 16.0 Week
salary Promoter = Salary 8.0 Day
salary MuckDuck = Salary 200.0 Month

instance randomListJob :: RandomList Job where
  randomList = filterA (const randomBool) [Admin, Promoter, MuckDuck]

--- food
data Food = SemiFinished | NormalFood | Cafe

instance showFood :: Show Food where
  show SemiFinished = "Полуфабрикаты"
  show NormalFood = "Купить нормальной еды"
  show Cafe = "Кафе"

instance hintFood :: Hint Food where
  time :: Food -> Time
  time SemiFinished = 2
  time NormalFood = 2
  time Cafe = 2

  price :: Food -> Money
  price SemiFinished = 2.0
  price NormalFood = 4.0
  price Cafe = 6.0

  mood :: Food -> Mood
  mood SemiFinished = 10
  mood NormalFood = 20
  mood Cafe = 40

-- rest
data Activity = Nap | SurfInternet | GetDrunk

instance showActivity :: Show Activity where
  show Nap = "Подремать"
  show SurfInternet = "Сидеть в интернете"
  show GetDrunk = "Бухать"

instance hintActivity :: Hint Activity where
  price :: Activity -> Money
  price Nap = 0.0
  price SurfInternet = 0.0
  price GetDrunk = 5.0

  time :: Activity -> Time
  time Nap = 1
  time SurfInternet = 4
  time GetDrunk = 2

  mood :: Activity -> Mood
  mood Nap = 10
  mood SurfInternet = 40
  mood GetDrunk = 80

-- project work

data ProjectWork = DuctTape | BuyAssets

instance showProjectWork :: Show ProjectWork where
  show DuctTape = "Делать через жопу"
  show BuyAssets = "Купить за долляры"

instance hintProjectWork :: Hint ProjectWork where
  price :: ProjectWork -> Money
  price DuctTape = 0.0
  price BuyAssets = 200.0

  time :: ProjectWork -> Time
  time DuctTape = 4
  time BuyAssets = 0

  mood :: ProjectWork -> Mood
  mood DuctTape = -30
  mood BuyAssets = 0

--  project

type Progress = Int
type Efforts = { time :: Time, mood :: Mood, progress :: Progress }

class Eq a <= Task a where
  efforts :: a -> Efforts
  list :: Array a
  toProjectTask :: a -> ProjectTask
  desc :: a -> String
  condition :: a -> String
  canStart :: Project -> a -> Boolean

data Idea = Story | DesignDoc | Hero | Plot | Enemies
data Engine = DownloadUnity
            | LearnInterface
            | LearnBasics
            | LearnAnimation
            | LearnPhysics
            | LearnCamera
            | StartEngine
            | DoAssets
            | DoAnimation
            | DoPhysics
            | DoLevelDesign
            | DoMainMenu
            | TuneGraphics
            | TuneMusic
            | Compile

data Graphics = DownloadPhotoshop
              | LearnDrawingForms
              | LearnDrawingPerspective
              | LearnDrawingShadowing
              | LearnDrawingColor
              | DrawHero
              | DrawEnv
              | DrawEnemies

data Music = SearchMusicSoft | DoMusic

derive instance eqIdea :: Eq Idea
derive instance eqEngine :: Eq Engine
derive instance eqGraphics :: Eq Graphics
derive instance eqMusic :: Eq Music

instance taskIdea :: Task Idea where
  list = [Story, DesignDoc, Hero, Plot, Enemies]

  desc = case _ of
    Story -> "Придумать идею"
    DesignDoc -> "Расписать дизайн документ"
    Hero -> "Придумать главного героя"
    Plot -> "Придумать сюжет"
    Enemies -> "Придумать врагов"

  efforts _ = { time: 1, mood: 20, progress: 20 }

  condition _ = ""
  canStart _ _ = true
  toProjectTask _ = Idea

instance taskEngine :: Task Engine where
  list = [ DownloadUnity
         , LearnInterface
         , LearnBasics
         , LearnAnimation
         , LearnPhysics
         , LearnCamera
         , StartEngine
         , DoAssets
         , DoAnimation
         , DoPhysics
         , DoLevelDesign
         , DoMainMenu
         , TuneGraphics
         , TuneMusic
         , Compile
         ]

  desc = case _ of
    DownloadUnity -> "Скачать Юнити"
    LearnInterface -> "Смотреть урок #1 Знакомство с интерфейсом"
    LearnBasics -> "Смотреть урок #2 Первые шаги"
    LearnAnimation -> "Смотреть урок #3 Анимация"
    LearnPhysics -> "Смотреть урок #4 Физика объектов"
    LearnCamera -> "Смотреть урок #5 Камера"
    StartEngine -> "Начать Делать"
    DoAssets -> "Таскать ассеты"
    DoAnimation -> "Настроить анимацию"
    DoPhysics -> "Настроить Физику"
    DoLevelDesign -> "Делать лэвел дизайн"
    DoMainMenu -> "Делать Главное меню"
    TuneGraphics -> "Настройка Графики"
    TuneMusic -> "Настройка Музыки"
    Compile -> "Компиляровать игру"

  condition StartEngine = "Нужна полностью готовая идея"
  condition _ = ""

  efforts = case _ of
    DownloadUnity -> { time: 2, mood: 20, progress: 1 }
    LearnInterface -> { time: 3, mood: 30, progress: 2 }
    LearnBasics -> { time: 3, mood: 40, progress: 3 }
    LearnAnimation -> { time: 3, mood: 50, progress: 4 }
    LearnPhysics -> { time: 3, mood: 60, progress: 5 }
    LearnCamera -> { time: 3, mood: 70, progress: 5 }
    StartEngine -> { time: 4, mood: 80, progress: 5 }
    DoAssets ->  { time: 4, mood: 60, progress: 5 }
    DoAnimation -> { time: 5, mood: 80, progress: 10 }
    DoPhysics -> { time: 6, mood: 80, progress: 10 }
    DoLevelDesign -> { time: 6, mood: 80, progress: 10 }
    DoMainMenu -> { time: 5, mood: 80, progress: 10 }
    TuneGraphics -> { time: 4, mood: 50, progress: 10 }
    TuneMusic -> { time: 4, mood: 50, progress: 10 }
    Compile -> { time: 10, mood: 99, progress: 10 }

  canStart project StartEngine = isNothing project.idea
  canStart project TuneGraphics = isNothing project.graphics
  canStart project TuneMusic = isNothing project.music
  canStart _ _ = true

  toProjectTask _ = Engine


hasIdea :: Project -> Idea -> Boolean
hasIdea project idea = case project.idea of
  Nothing -> true
  Just idea' -> let
    all = list :: Array Idea
    current = unsafePartial $ fromJust $ idea' `elemIndex` all
    index = unsafePartial $ fromJust $ idea `elemIndex` all
    in current >= index

instance taskGraphics :: Task Graphics where
  list = [ DownloadPhotoshop
         , LearnDrawingForms
         , LearnDrawingPerspective
         , LearnDrawingShadowing
         , LearnDrawingColor
         , DrawHero
         , DrawEnv
         , DrawEnemies
         ]
  desc = case _ of
    DownloadPhotoshop -> "Скачать Фотошоп"
    LearnDrawingForms -> "Учиться рисованию: Форма"
    LearnDrawingPerspective -> "Учиться рисованию: Перспектива"
    LearnDrawingShadowing -> "Учиться рисованию: Светотень"
    LearnDrawingColor -> "Учиться рисованию: Цвет"
    DrawHero -> "Рисовать главного героя"
    DrawEnv -> "Рисовать окружение"
    DrawEnemies -> "Рисовать врагов"

  condition DrawHero = "Нужно придумать главного героя"
  condition DrawEnv = "Нужно придумать сюжет"
  condition DrawEnemies = "Нужно придумать врагов"
  condition _ = ""

  canStart project task = case task of
    DrawHero ->  project `hasIdea` Hero
    DrawEnv -> project `hasIdea` Plot
    DrawEnemies -> project `hasIdea` Enemies
    _ -> true

  efforts DownloadPhotoshop = { time: 2, mood: 20, progress: 2 }
  efforts LearnDrawingForms = { time: 5, mood: 60, progress: 2 }
  efforts LearnDrawingPerspective = { time: 5, mood: 60, progress: 2 }
  efforts LearnDrawingShadowing = { time: 5, mood: 60, progress: 2 }
  efforts LearnDrawingColor = { time: 5, mood: 60, progress: 2 }
  efforts DrawHero = { time: 10, mood: 99, progress: 30 }
  efforts DrawEnv = { time: 10, mood: 99, progress: 30 }
  efforts DrawEnemies = { time: 10, mood: 99, progress: 30 }
  toProjectTask _ = Graphics

instance taskMusic :: Task Music where
  list =  [SearchMusicSoft, DoMusic]
  desc = case _ of
    SearchMusicSoft -> "Искать софт"
    DoMusic -> "Пытаться делать"

  canStart project DoMusic = project `hasIdea` Plot
  canStart _ _ = true

  condition DoMusic = "Нужна идея"
  condition _ = ""
  efforts SearchMusicSoft = { time: 1, mood: 20, progress: 1 }
  efforts DoMusic = { time: 1, mood: 20, progress: 99 }
  toProjectTask _ = Music

type Project = { idea :: Maybe Idea
               , engine :: Maybe Engine
               , graphics :: Maybe Graphics
               , music :: Maybe Music
               }

emptyProject :: Project
emptyProject = { idea: Just Story
               , engine: Just DownloadUnity
               , graphics: Just DownloadPhotoshop
               , music: Just SearchMusicSoft
               }

instance hintIdea :: Hint Idea where
  price _ = 0.0
  time _ = 0
  mood _ = 20

data ProjectTask
  = Idea
  | Engine
  | Graphics
  | Music

-- misc

showMood :: Mood -> String
showMood m
  | m > 0 = " ☺" <> (show m)
  | m < 0 = " ☹" <> (show $ -m)
  | otherwise = ""

hintShow :: forall a. Hint a => a -> String
hintShow x = (show $ price x) <> "$" <> " " <> (show $ time x) <> "ч." <> (showMood $ mood x)
