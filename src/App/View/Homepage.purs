module App.View.Homepage where

import App.Events (Event(..), Event(..))
import App.Game (endDate)
import App.Player (Player(..), daysToSalary, dead, limit, nextSalaryDate, outOfTime)
import App.Project (currentProgress, finished, totalProgress)
import App.State (Achievement, Difficulty(..), Menu(..), Stage(..), State, Status(..))
import App.Types (class Task, Idea(..), Project, ProjectTask(..), ProjectWork(..), canStart, condition, desc, efforts, salary, toProjectTask)
import App.View.Dialog (dialog)
import App.View.Error (error)
import App.View.Menu (menu)
import App.View.ProgressBar (progressBar)
import CSS (Key(..), dodgerblue, key, pct, table, width)
import Control.Alt ((<|>))
import Data.DateTime (Millisecond, diff)
import Data.Either (fromRight)
import Data.Enum (class Enum)
import Data.Formatter.DateTime as FDT
import Data.Int (ceil, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Time.Duration (Days(..), Milliseconds(..))
import Partial.Unsafe (unsafePartial)
import Prelude (unit)
import Prelude hiding (div,id)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, audio, button, div, h1, hr, img, source, span)
import Text.Smolder.HTML.Attributes (autoplay, className, controls, href, id, loop, src, style, target, title)
import Text.Smolder.Markup (MarkupM(..), text, (!), (!?), (#!))

stats :: State -> HTML Event
stats { player: pl, animation, status } =
  div ! id "stats" $ do
    div $ text $ "Имя: " <> pl.name
    div
      ! className "stats-stat"
      #! onClick (const $ OpenMenu Food)
      $ do
      span $ do
        icon "☕"
        text $ "Сытость:"
      progressBar { current: pl.fullness, max: limit }
    div
      ! className "stats-stat"
      #! onClick (const $ OpenMenu Rest)
      $ do
      span $ do
        icon "☻"
        text $ "Настроение: "
      progressBar { current: pl.mood, max: limit }
    div ! className "stats-stat" $ do
      (span
       !? (animation == Just "money")) (className "income") $ do
        icon "$"
        text $ "Деньги: " <> show pl.money
      text ""
    hr
    div $ text $ "Работа: " <> show pl.job <> " (" <> (show $ salary pl.job) <> ")"
    case daysToSalary pl of
      Nothing -> Return unit
      Just days -> div $ text ("Дней до зарплаты: " <> show days)
    div $ text $ "Статус: " <> (maybe "Прокрастинация" show status)

icon :: String -> HTML Event
icon s = div ! className "icon" $ text s

watch :: Player -> HTML Event
watch pl = div
           ! id "watch"
           ! title "Часики тикают. Клик чтобы проебать день."
           #! onClick (const SkipDay)
           $ text $ show left <> "/24" where
  {left, max} = pl.freetime

date :: Player -> HTML Event
date {date} = div ! id "date" $ do
  div $ text $ "Дата: " <> (unsafePartial $ fromRight $ FDT.formatDateTime "DD.MM.YYYY" date)
  div $ text $ "До TWG " <> days <> " дней"
  where
    (Days n) = diff endDate date
    days = show $ ceil n

food :: HTML Event
food = button
       ! id "food"
       ! title "Еда"
       #! onClick (const $ OpenMenu Food)
       $ text ""

rest :: HTML Event
rest = button
       ! id "rest"
       ! title "Отдых"
       #! onClick (const $ OpenMenu Rest)
       $ text ""

work :: HTML Event
work = button
       ! id "job"
       ! title "Работа"
       #! onClick (const $ OpenMenu WorkOptions)
       $ text ""

actions :: HTML Event
actions = div ! id "actions" $ do
  food
  rest
  work

projectTask :: forall a. Task a => String -> String -> Maybe a -> Project -> HTML Event
projectTask icon name task project =
  div ! className "project-task" $ do
    div
      ! className "project-task-name"
      #! onClick (const click)
      $ do
        img ! className "project-task-icon" ! src ("assets/icons/" <> icon <> ".png")
        div ! className "project-task-button" $ text name
        div ! className "project-task-desc" $ do
          div ! className "project-desc" $ text $ maybe "Готово" desc task
          div ! className "project-desc-condition" $ text $ maybe "" condition task
        case task of
          Nothing -> Return unit
          Just task -> let {time, mood} = efforts task in
        div ! className "project-efforts" $ text  (show time <> "ч. ☹" <> show mood)
    div ! className "project-task-progress" $ text $ (show $ currentProgress task) <> "%"
  where
    click = case task of
      Nothing -> CloseDialog
      Just task -> if canStart project task
                   then Wait
                        (Milliseconds $ toNumber $ (efforts task).time * 1000)
                        Working
                        $ StartProjectTask (toProjectTask task) DuctTape
                   else CloseDialog

project :: Project -> String -> HTML Event
project pr query = do
  div ! id "project-header" $ do
    div ! id "project-progress" $ do
      div
        ! id "project-progress-full"
        ! style ("width: " <> (show $ totalProgress pr) <> "%")
        ! title ((show $ totalProgress pr) <> "%")
        $ span $ text "C :\\ Заготовочка"
    div ! id "project-search" $ do
      div ! className "icon" $ text "⚲"
      text query
  div ! id "project-contents" $ do
    projectTask "idea" "Идея" pr.idea pr
    projectTask "engine" "Движок" pr.engine pr
    projectTask "graphics" "Графика" pr.graphics pr
    projectTask "music" "Музыка" pr.music pr
  div
    ! id "project-close"
    ! className "close-button"
    #! onClick (const $ OpenMenu Suicide)
    $ text "✖"

gameOver :: State -> Maybe (HTML Event)
gameOver { player: pl, difficulty } = death <|> win where
  death :: Maybe (HTML Event)
  death = dead pl <#> \reason ->
    dialog "Пиздец" $ do
      img ! src "assets/death-1.png"
      div ! id "game-over" $ text reason
      button #! onClick (const restart) $ text "Мкай"
      where
        restart
          | outOfTime pl = Restart
          | otherwise = case difficulty of
            Anon -> Restart
            Abu -> Revive

  win :: Maybe (HTML Event)
  win
    | finished pl.project =
      Just $ dialog "Заебись" $ do
        -- div ! id "game-win" $ text "Я прошел эту сраную игру и все что я получил..."
        -- button #! onClick (const Restart) $ text "Нихуя"
        div ! id "game-win" $ text "Я забыл написать ридми..."
        button #! onClick (const Restart) $ text "Пиздарики"
    | otherwise =  Nothing

achievement :: Achievement -> HTML Event
achievement { name, file } = dialog "Тадам!" $ do
  img ! src ("assets/achievements/" <> file)
  div ! id "achievement-desc" $ text name

status :: Status -> HTML Event
status s = dialog (show s) $ do
  div ! id "spinner" $ img ! src "assets/spinner.png"

view :: State -> HTML Event
view st = do
    img ! id "logo" ! src "assets/logo.png"
    div ! id "main" $ do
      stats st
      actions
    div ! id "time" $ do
      watch st.player
      date st.player
    div ! id "project" $ do
      project st.player.project st.searchQuery

    case gameOver st of
      Just end -> end
      Nothing -> do
        maybe (Return unit) achievement st.achievement
        maybe (Return unit) error st.error
        maybe (Return unit) status st.status
        case st.menu of
          Nothing -> (Return unit)
          Just m -> menu m st.random
