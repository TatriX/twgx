module App.State where

import App.Config (config)
import App.Player (Player, newPlayer)
import App.Routes (Route, match)
import App.Types (class Hint, class RandomList, class Task, Engine(..), Job, ProjectTask)
import Data.Maybe (Maybe, Maybe(..))
import Data.Show (class Show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)

data Menu = Food | Rest | WorkOptions | Work | Suicide

data Status = SearchingJob | LookingForIdea | Working | Waiting | Status String

instance showStatus :: Show Status where
  show SearchingJob = "Ищем работу"
  show LookingForIdea = "Думаем над идеей"
  show Working = "Въебываем-с"
  show Waiting = "Ждем-с"
  show (Status s) = s

type RandomLists = { jobs :: Array Job }

type Achievement = { name :: String, file :: String }

data Stage = MainMenu | Game Difficulty
data Difficulty = Anon | Abu

type State =
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , player :: Player
  , menu :: Maybe Menu
  , random :: RandomLists
  , searchQuery :: String
  , error :: Maybe String
  , achievement :: Maybe Achievement
  , status :: Maybe Status
  , animation :: Maybe String
  , stage :: Stage
  , showWebm :: Boolean
  , difficulty :: Difficulty
  }

init :: String -> State
init url =
  { title: config.title
  , route: match url
  , loaded: false
  , player: newPlayer "Илья"
  , menu: Nothing
  , random: {jobs: []}
  , error: Nothing
  , achievement: Nothing
  , status: Nothing
  , animation: Nothing
  , searchQuery: "ецп"
  , stage: MainMenu
  , showWebm: false
  , difficulty: Anon
  }
