module App.View.Menu (menu) where

import App.Types

import App.Events (Event(..))
import App.Food (satiety)
import App.Job (workTime)
import App.State (Menu(..), RandomLists, Status(..))
import App.View.Dialog (dialog)
import Data.Foldable (for_)
import Data.Time.Duration (Milliseconds(..))
import Prelude hiding (div,id)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (br, li, span, strong, ul)
import Text.Smolder.HTML.Attributes (title)
import Text.Smolder.Markup (text, (!), (#!))

menu :: Menu -> RandomLists -> HTML Event
menu Food _ = dialog "Еда" do
  ul $ for_ [SemiFinished, NormalFood, Cafe] item
    where item food = li #! onClick (const (Eat food)) $ do
            strong $ text $ show food
            span $ text $ hintShow food <> " ☕" <> (show $ satiety food)


menu Rest _ = dialog "Отдых" do
  ul $ for_ [Nap, SurfInternet, GetDrunk] item
    where item activity = li #! onClick (const (TakeRest activity)) $ do
            strong $ text $ show activity
            span $ text $ hintShow activity

menu WorkOptions _ = dialog "Свесив ножки я решил" $ do
  ul $ do
    li #! onClick (const $ Wait (Milliseconds 1000.0) SearchingJob SearchJob) $ do
      strong $ text "Искать работу/Уволиться"
      span $ text "1ч. 20☹"
    li #! onClick (const $ Wait (Milliseconds 1000.0) (Status "Звоним мамуле") AskMommy) $ do
      strong $ text "Просить денег у мамки"
      span $ text "Раз в неделю"

menu Work {jobs} = dialog "Работа" do
  ul $ for_  ([Jobless] <> jobs) item
    where item job = li #! onClick (const (TakeJob job)) $ do
            strong $ text $ show job
            span $ text $ show $ salary job
            br
            span $ text $ (show $ workTime job) <> "ч. в день"

menu Suicide _ = dialog "Выпилится" do
  ul $ for_  ["Повесится", "Положить ебало под поезд"] method
    where method msg = li #! onClick (const $ Wait (Milliseconds 1000.0) (Status msg) CommitSuicide) $ do
            strong $ text $ msg
