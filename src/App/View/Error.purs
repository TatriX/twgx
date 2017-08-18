module App.View.Error (error) where

import App.Events (Event)
import App.View.Dialog (dialog)
import Prelude hiding (div,id)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className, id)
import Text.Smolder.Markup (text, (!))

error :: String -> HTML Event
error err = dialog "Ой" do
  div ! className "error" $ text err
