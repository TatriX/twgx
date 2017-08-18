module App.View.MainMenu where

import App.Events (Event(..))
import App.State (Difficulty(..), Stage(..), State)
import Data.Maybe (maybe)
import Prelude hiding (div,id)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, img, source, video)
import Text.Smolder.HTML.Attributes (autoplay, className, id, loop, src)
import Text.Smolder.Markup (MarkupM(..), text, (!), (#!))

webm :: Boolean -> HTML Event
webm false = Return unit
webm true = video
            ! autoplay "true"
            ! loop "true"
            $ source ! src "assets/dont.webm"


view :: State -> HTML Event
view st = do
  img ! id "logo" ! src "assets/logo.png"
  div ! id "main-menu" $ do
    webm st.showWebm
    div
      ! className "main-menu-button"
      #! onClick (const $ SetStage (Game Abu))
      $ text "Я Абу"
    div
      ! className "main-menu-button"
      #! onClick (const $ SetStage (Game Anon))
      $ text "Играть"
    div
      ! className "main-menu-button"
      #! onClick (const $ ToggleWebm)
      $ text "Не играть"
