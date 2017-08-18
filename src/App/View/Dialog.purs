module App.View.Dialog (dialog) where

import App.Events (Event(..), Event(..))
import DOM.Event.Event (currentTarget, target)
import DOM.Node.Node (isEqualNode)
import Prelude hiding (div,id)
import Pux.DOM.Events (DOMEvent, onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className, id)
import Text.Smolder.Markup (text, (!), (#!))

type Title = String

dialog :: Title -> HTML Event -> HTML Event
dialog title markup =
  div ! id "dialog-overlay" #! onClick MaybeCloseDialog $ do
    div ! id "dialog"  $ do
      div ! id "dialog-title" $ text title
      closeButton
      div ! id "dialog-content" $ markup

closeButton :: HTML Event
closeButton = div
        ! className "close-button"
        #! onClick (const CloseDialog)
        $ text "✖"
