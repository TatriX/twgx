module App.View.Layout where

import App.Events (Event)
import App.Routes (Route(NotFound, Home))
import App.State (Stage(..), State)
import App.View.Homepage as Homepage
import App.View.MainMenu as MainMenu
import App.View.NotFound as NotFound
import Prelude (($))
import Prelude hiding (div,id)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, audio, div, source, span)
import Text.Smolder.HTML.Attributes (autoplay, className, controls, href, id, loop, src, target)
import Text.Smolder.Markup (text, (!))

sounds :: HTML Event
sounds = do
  div ! id "music-controls" $ do
    audio
      ! id "main-music"
      -- ! autoplay "autoplay"
      ! controls "controls"
      ! loop "loop"
      $ source ! src "assets/music/5048.mp3"
    audio
      ! id "click-sound"
      $ source ! src "assets/music/click.wav"
    audio
      ! id "tada-sound"
      $ source ! src "assets/music/tada.wav"
    span $ text "♫"

view :: State -> HTML Event
view st = do
  div ! className "app" $ do
    sounds
    case st.route of
      (Home _) -> case st.stage of
                       MainMenu -> MainMenu.view st
                       (Game _) -> Homepage.view st
      (NotFound url) -> NotFound.view st
  a
    ! id "author"
    ! href "https://github.com/tatrix"
    ! target "_blank"
    $ text "TatriX"
