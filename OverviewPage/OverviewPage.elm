module OverviewPage.OverviewPage exposing (Model, Msg(UpdateTeamData, UpdateTopTenData), init, update, view)

import Html exposing (Html, text, h2, div, button)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)

import Types exposing (..)
import Helpers as H exposing (toFixed)

import OverviewPage.Chart exposing (barChart, doubleBarChart, pitchChart, mapChart)
import OverviewPage.TopTenSection exposing (topTenSection)



{-------
  MODEL
--------}

type alias Model =
  { teams : List TeamData
  , topTen : List TopTenData
  , activeChart : ChartType
  }


type ChartType = Money | Players


type Msg
  = UpdateTeamData (List TeamData)
  | UpdateTopTenData (List TopTenData)
  | UpdateActiveChart ChartType


-- Initialise with blank data
init : Model
init =
  Model [] [] Money



{--------
  UPDATE
---------}

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateTeamData newData ->
      { model | teams = newData }

    UpdateTopTenData newData ->
      { model | topTen = newData }

    UpdateActiveChart newChart ->
      { model | activeChart = newChart }



{------
  VIEW
-------}

-- We should definitely refactor this
view : Model -> Html Msg
view model =
  let
    -- Group all of the transfers together
    allTransfers =
      List.concat <| List.map .transfers model.teams

    -- Group the transfers by team
    transfersByTeam =
      List.map (\team -> (team.name, team.transfers)) model.teams

    -- Sort by decreasing value
    sort =
      List.reverse << List.sortBy snd

    -- Convert float values to ints
    convertInts =
      List.map (\t -> (fst t, toFloat (snd t)))

    -- For a given field, get the frequencies of its values. Then sort them and convert to ints
    getFrequenciesForField field =
      convertInts <| sort <| calculateFieldValueFrequencies allTransfers field

    -- Retrieve the values for use in the football pitch chart
    pitchChartValues =
      List.filter (\f -> fst f /= "-") <| getFrequenciesForField .position

    -- Calculate the sets of values for the different bar charts
    barChartValues = 
      [ (convertInts <| calculateAgeCategoryFrequencies allTransfers)
      , getFrequenciesForField .signedFrom
      ]

    -- Combine the bar chart values and configs, and output the charts
    barCharts =
      List.map barChart <| barChartConfigs barChartValues

    -- Which double bar chart config to use
    doubleConfigToUse =
      List.map fst <| List.filter (\c -> snd c == model.activeChart) doubleBarChartConfigs

    -- Which double bar chart values to use
    doubleValuesToUse =
      case (model.activeChart) of
        Money ->
          calculateMoneyTotals transfersByTeam

        Players ->
          calculatePlayerTotals transfersByTeam

    -- The double bar chart (config plus values) to display 
    doubleBarChartToDisplay =
      List.map doubleBarChart <| List.map2 (\c v -> { c | items = v }) doubleConfigToUse [ doubleValuesToUse ]

    -- HTML for the buttons to toggle between charts
    doubleBarChartButtons =
      let
        btn label chartType =
          div
            [ onClick <| UpdateActiveChart chartType
            , classList [("active", model.activeChart == chartType), ("button", True)]
            ]
            [ h2 [] [ text label ]
            ]

      in
        [ btn "Money" Money
        , btn "Players" Players
        ]

  in 
    div [ class "page overview" ]
      [ div [ class "charts-top" ]
          [ div [ class "buttons" ] doubleBarChartButtons
          , div [ class "double-bar-charts" ] doubleBarChartToDisplay
          ]
      , topTenSection model.topTen
      , div [ class "charts-bottom" ]
          [ div [ class "first-container" ]
              [ drawMapChart allTransfers
              ]
          , div [ class "second-container" ]
              [ Maybe.withDefault (text "") (List.head barCharts)
              , pitchChart <| pitchChartConfig pitchChartValues
              , Maybe.withDefault (text "") (List.head <| List.drop 1 barCharts)
              ]
          ]
      ]


-- Display the world map chart
drawMapChart : List TransferData -> Html Msg
drawMapChart transfers =
  let
    heading : String
    heading =
      "Nationality of incoming players"

    -- Slim down the transfer data to only that which is valid and which we need
    slimData : List ( String, String )
    slimData =
      let
        exists str =
          case str of
            Just value ->
              True

            Nothing ->
              False

        extract (n, r) =
          ( Maybe.withDefault "" n, Maybe.withDefault "" r )

      in
        List.map (\t -> (t.region, t.nationality)) transfers
          |> List.filter (exists << fst)
          |> List.filter (exists << snd)
          |> List.map extract

    -- Given a transfer and the existing items, either add the transfer to an existing item or create a new one
    reduce : ( String, String ) -> MapChartItems -> MapChartItems
    reduce (region, nat) items =
      if List.isEmpty (List.filter (\(r, n, i) -> n == nat) items)
      then (region, nat, 1) :: items
      else List.map (\(r, n, i) -> if n == nat then (r, n, i + 1) else (r, n, i)) items

    -- The items to pass to the map chart function
    mapItems : MapChartItems
    mapItems =
      List.foldr reduce [] slimData

    -- The full config to pass to the map chart function
    config : MapChartOptions
    config =
      { heading = heading
      , items = mapItems
      }

  in
    mapChart config


-- Increment the count for a given label, if it exists, or else start a new count at 1
incremementCountOrStartNew : String -> List (String, Int) -> List (String, Int)
incremementCountOrStartNew itemLabel items =
  let
    updateItem ((label, count) as item) =
      if label == itemLabel
      then (label, count + 1)
      else item

  in
    if List.any (\(label, _) -> label == itemLabel) items
    then List.map updateItem items
    else (itemLabel, 1) :: items


-- Add up the number of transfers for each unique value of a given field.
-- For example, for 'Nationality', add up the number of players of each nationality.
calculateFieldValueFrequencies : List TransferData -> (TransferData -> Maybe String) -> List (String, Int)
calculateFieldValueFrequencies transfers accessField =
  let
    parse transfer items =
      case (accessField transfer) of
        Just item ->
          incremementCountOrStartNew item items

        Nothing ->
          items

  in
    List.foldr parse [] transfers


-- Group the player ages from the transfers data
calculateAgeCategoryFrequencies : List TransferData -> List (String, Int)
calculateAgeCategoryFrequencies transfers =
  let
    -- Set up the groupings as (category, count, min, max)
    initialGroups =
      [ ("16-21", 0, 16, 21)
      , ("22-24", 0, 22, 24)
      , ("25-29", 0, 25, 29)
      , ("30+", 0, 30, 100)
      ]

    -- Given an age and a group, update that group if appropriate
    updateGroup age ((category, count, min, max) as group) =
      if age >= min && age <= max
      then (category, count + 1, min, max)
      else group
        
    -- If we have the player age, update the groups
    parse transfer groups =
      case (.age transfer) of
        Just a ->
          List.map (updateGroup a) groups

        Nothing ->
          groups

  in
    List.map (\(category, count, _, _) -> (category, count)) <| List.foldr parse initialGroups transfers


-- Return only those a transfers that are 'In'
onlyIn : List TransferData -> List TransferData
onlyIn =
  List.filter (\t -> t.transferType == In)


-- Return only those a transfers that are Out'
onlyOut : List TransferData -> List TransferData
onlyOut =
  List.filter (\t -> t.transferType == Out)


-- Calculate the total amount of money spent/received for each team
calculateMoneyTotals : List (String, List TransferData) -> DoubleBarChartItems
calculateMoneyTotals teamTransfers =
  let
    sumFees =
      H.toFixed 2 << List.sum << List.map (Maybe.withDefault 0 << .feeNum)

    parseTeam (name, transfers) =
      ( name
      , sumFees <| onlyIn transfers
      , sumFees <| onlyOut transfers
      )

  in
    List.map parseTeam teamTransfers


-- Calculate the total number of players in/out for each team
calculatePlayerTotals : List (String, List TransferData) -> DoubleBarChartItems
calculatePlayerTotals teamTransfers =
  let
    parseTeam (name, transfers) =
      ( name
      , toFloat <| List.length <| onlyIn transfers
      , toFloat <| List.length <| onlyOut transfers
      )

  in
    List.map parseTeam teamTransfers


-- Configuration options for the double bar charts
doubleBarChartConfigs : List (DoubleBarChartOptions, ChartType)
doubleBarChartConfigs =
  [ (
      { className = "total-money"
      , formatPrefix = "£"
      , formatSuffix = "m"
      , totalLabelA = "Spent"
      , totalLabelB = "Received"
      , items = []
      }
    , Money)
  , (
      { className = "total-players"
      , formatPrefix = ""
      , formatSuffix = ""
      , totalLabelA = "In"
      , totalLabelB = "Out"
      , items = []
      }
    , Players)
  ]


-- Configuration options for the bar charts
barChartConfigs : List BarChartItems -> List BarChartOptions
barChartConfigs itemsList =
  let
    options =
      [ { heading = "Age of incoming players"
        , className = "age"
        , items = []
        }
      , { heading = "Where players were signed from"
        , className = "signed"
        , items = []
        }
      ]

  in
    List.map2 (\o i -> { o | items = i }) options itemsList


-- Configuration options for the pitch chart
pitchChartConfig : BarChartItems -> BarChartOptions
pitchChartConfig items =
  { heading = "Position of incoming players"
  , className = "position"
  , items = items
  }
