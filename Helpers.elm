module Helpers exposing (..)

import Regex exposing (..)
import String exposing (toLower)


-- Slugify a string
slugify : String -> String
slugify =
  String.toLower
    >> replace All (regex "\\s+") (\_ -> "-")
    >> replace All (regex "[^\\w\\-]+") (\_ -> "")
    >> replace All (regex "\\-\\-+") (\_ -> "-")
    >> replace All (regex "^-+") (\_ -> "")
    >> replace All (regex "-+$") (\_ -> "")


-- Round a floating point number to a given number of decimal places
toFixed : Int -> Float -> Float
toFixed numberDecimals input =
  let
    multiplier =
      10 ^ numberDecimals

  in
    (toFloat <| round <| input * multiplier) / multiplier
