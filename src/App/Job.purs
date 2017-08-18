module App.Job where

import App.Types
import Prelude

import App.Player (Player(..), defaultFreeTime, nextSalaryDate)
import App.Types (Job(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomBool, randomInt)
import Data.Array (filter, filterA)
import Data.Either (Either(..))

workTime :: Job -> Int
workTime  = case _ of
      Jobless -> 0
      Admin -> 3
      Promoter -> 6
      MuckDuck -> 8

getJob :: Player -> Job -> Either String Player
getJob pl job = Right $ pl { job = job
                           , salaryDate = nextSalaryDate job pl.date
                           , freetime { max = defaultFreeTime.max - workTime job }
                           }
