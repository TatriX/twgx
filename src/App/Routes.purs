module App.Routes where

import Prelude

data Route = Home String | NotFound String

match :: String -> Route
match url = Home url

toURL :: Route -> String
toURL (NotFound url) = url
toURL (Home url) = "/" <> url
