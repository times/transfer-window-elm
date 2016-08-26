module Countdown.Countdown exposing (Model, Msg, init, update, view)

import Html exposing (Html, text, span)
import Time exposing (..)
import Date exposing (..)



{-------
  MODEL
--------}

type alias Model = Time


-- Provide a (somewhat arbitrary) time as a starting value
init : Model
init =
  1469200719085



{--------
  UPDATE
---------}

type alias Msg = Time


-- Update the countdown with a new time
update : Msg -> Model -> Model
update newTime model =
  newTime



{------
  VIEW
-------}

-- Output the current countdown time in HTML
view : Model -> Html a
view model =
  span [] [ text <| formatCountdown model ]


-- Convert the current countdown time into a nicely formatted string
formatCountdown : Model -> String
formatCountdown currentTime =
  let
    -- The current date
    currentDate =
      Date.fromTime currentTime

    -- The date on which the transfer window ends
    endDate =
      Result.withDefault currentDate <| Date.fromString "2016-08-31T23:00:00"

    -- The amount of time until the end of the window
    timeDiff =
      (Date.toTime endDate) - currentTime

    -- The number of days remaining
    days =
      floor <| (inHours timeDiff / 24)

    -- The number of extra hours remaining
    hours =
      (floor <| inHours timeDiff) `rem` 24

    -- The number of extra minutes remaining
    minutes =
      (floor <| inMinutes timeDiff) `rem` 60

    -- The number of extra seconds remaining
    seconds =
      (floor <| inSeconds timeDiff) `rem` 60

    timings =
      [ days, hours, minutes, seconds ]

    labels =
      [ "day", "hour", "minute", "second" ]

  in 
    List.map2 (\t l -> (toString t) ++ " " ++ (if t == 1 then l else l ++ "s")) timings labels
      |> List.intersperse ", "
      |> List.foldr (++) ""
