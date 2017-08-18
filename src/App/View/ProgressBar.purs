module App.View.ProgressBar where

import App.Events (Event)
import Prelude hiding (div)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, span)
import Text.Smolder.HTML.Attributes (className, style, title)
import Text.Smolder.Markup (text, (!))

progressBar :: { current :: Int, max :: Int } -> HTML Event
progressBar { current, max } =
  div
    ! className "progress-bar"
    ! (title txt)
    $ div
        ! className "progress-bar-full"
        ! style ("width: " <> (show width) <> "%")
        $ span $ text txt
  where
    width = (current * 100) / max
    txt = (show current) <> " / " <> (show max)
