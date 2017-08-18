module App.Events where

import App.Types
import Prelude

import App.Food (eat, randomFoodEvent)
import App.Game (randomSearchQuery)
import App.Job (getJob)
import App.Player (Player(..), RandomEvent, askMommy, revive, skipDay, update)
import App.Project (achievement, buyAssets, tryTask, workOnProject)
import App.Rest (randomRestEvent, rest)
import App.Routes (Route)
import App.Sound (SOUND, playTada)
import App.State (Achievement, Menu(..), Stage(..), State, Status(..), init)
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, error, logShow)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM, random, randomInt, randomRange)
import Control.MonadPlus (guard)
import DOM (DOM)
import DOM.Event.Event (currentTarget, target)
import DOM.Node.Node (isEqualNode)
import Data.Either (Either(..), either, hush, isRight)
import Data.Maybe (Maybe, Maybe(..), isJust, isNothing)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects, waitState)
import Pux.DOM.Events (DOMEvent)


data Event = PageView Route
           | SetStage Stage
           | Restart
           | OpenMenu Menu
           | Alert String
           | ApplyRandomEvent RandomEvent
           | ShowAchievement Achievement
           | Randomized Event {jobs :: Array Job}
           | Done Event
           | MaybeCloseDialog DOMEvent
           | CloseDialog
           | Wait Milliseconds Status Event
           | UpdateSearchQuery String Event
           | AskMommy
           | SearchJob
           | Eat Food
           | TakeRest Activity
           | TakeJob Job
           | StartProjectTask ProjectTask ProjectWork
           | ProjectTaskResult ProjectTask (Tuple (Maybe String) Player)
           | SkipDay
           | ToggleWebm
           | CommitSuicide
           | Revive

type AppEffects fx = (random :: RANDOM, sound :: SOUND, dom :: DOM | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) st =
  noEffects $ st { route = route, loaded = true }

foldp (SetStage stage) st =
  noEffects $ st' { stage = stage} where
  st' = case stage of
    (Game difficulty) -> st { difficulty = difficulty }
    otherwise -> st

foldp Restart _ =
  noEffects $ init ""

foldp Revive st =
  { state: st { player = revive st.player }
  , effects: [pure $ Just $ Alert "Тебя отресали. Но если что ты сдох."]
  }

foldp (Randomized ev {jobs}) st =
  { state: cleanup $ st { random { jobs = jobs } }
  , effects: [ pure $ Just ev ]
  }

foldp (Done ev) s =
  { state: cleanup s
  , effects: [ pure $ Just ev]
  }

foldp (Alert s) st =
  noEffects $ cleanup $ st { error = Just s }


foldp (ApplyRandomEvent event) st = let (Tuple msg pl) = event st.player in
  noEffects $ (cleanup st) { player = pl, error = Just msg}

foldp (ShowAchievement s) st =
  { state: cleanup $ st { achievement = Just s }
  , effects: [ do
                  playTada
                  pure Nothing
             ]
  }

foldp (UpdateSearchQuery query ev) st =
  { state: st { searchQuery = query }
  , effects: [ pure if isJust st.status
                  then Just ev
                  else Nothing
             ]
  }

foldp (Wait ms status ev) st =
  { state: (cleanup st) { status = Just status }
  , effects: [ do
                  delay ms
                  query <- liftEff randomSearchQuery
                  pure $ Just $ UpdateSearchQuery query ev
             ]
  }

foldp AskMommy st =
  noEffects $ (next st $ askMommy st.player 10.0) { animation = Just "money" }

foldp SearchJob st =
  { state: st { status = Just SearchingJob, player = update (-20) 0 st.player }
  , effects: [ do
                  list <- liftEff $ randomList
                  pure $ Just $ Randomized (OpenMenu Work) { jobs: list }
             ]
  }

foldp (OpenMenu menu) st =
  noEffects $ st { menu = Just menu }

foldp (MaybeCloseDialog ev) st
  | isJust st.status = noEffects st
  | otherwise = onlyEffects st [ do
                                    eq <- liftEff $ isEqualNode (target ev) (currentTarget ev)
                                    pure if eq then Just CloseDialog else Nothing
                               ]

foldp CloseDialog st =
  noEffects $ st { menu = Nothing, error = Nothing, achievement = Nothing, status = Nothing }
foldp (Eat food) st = let res = eat st.player food in
  { state: next st res
  , effects: [ generateRandomEvent res $ randomFoodEvent food ]
  }

foldp (TakeRest activity) st = let res = rest st.player activity in
  { state: next st res
  , effects: [ generateRandomEvent res $ randomRestEvent activity ]
  }

foldp (TakeJob job) st =
  noEffects $ next st $ getJob st.player job

foldp (StartProjectTask task BuyAssets) st =
  noEffects $ next st $ buyAssets st.player task

foldp (StartProjectTask task DuctTape) st =
  { state: cleanup $ st { status = Just LookingForIdea }
  , effects: [ do
                  r <- liftEff $ randomInt 0 100
                  pure $ Just $ ProjectTaskResult task $ tryTask st.player task r
             ]}

foldp (ProjectTaskResult task (Tuple err pl)) st =
  { state: st { player = pl}
  , effects: [
       pure $
       Alert <$> err
       <|>
       ShowAchievement <$> achievement st.player task
       ]
  }

foldp SkipDay st =
  noEffects st { player = skipDay st.player }

foldp ToggleWebm st =
  noEffects st { showWebm = not st.showWebm }

foldp CommitSuicide st =
  noEffects st { player = update (-pl.mood) 0 pl  } where
    pl = st.player

next :: State -> Either String Player -> State
next st (Left err) = cleanup $ st { error = Just err }
next st (Right pl) = cleanup $ st { player = pl}

generateRandomEvent ::
  forall eff
  . Either String Player
  -> Aff (random :: RANDOM | eff) (Maybe RandomEvent)
  -> Aff (random :: RANDOM | eff) (Maybe Event)
generateRandomEvent res generate = case res of
  Left _ -> pure Nothing
  Right _ -> do
    rev <- generate
    pure $ ApplyRandomEvent <$> rev

cleanup :: State -> State
cleanup st = st { menu = Nothing,  status = Nothing, animation = Nothing }
