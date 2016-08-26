module OverviewPage.Chart exposing (barChart, doubleBarChart, pitchChart, mapChart)

import Html exposing (..)
import Html.Attributes exposing (class, style, src)
import String exposing (toLower)

import Types exposing (BarChartOptions, DoubleBarChartOptions, MapChartOptions)
import Helpers as H


-- Helper function to get the maximum value in a list
maxValueInList : List Float -> Float
maxValueInList =
  Maybe.withDefault 0 << List.maximum


-- HTML for a set of bars
bars : Bool -> Bool -> Float -> List Float -> Html a
bars displayExtraRow displayLabels maxValue values =
  let
    -- HTML for a single bar
    bar value =
      div [ class "bar-container" ]
        [ div [ class "bar", style
            [ ("flex-basis", barWidth value maxValue)
            , ("-webkit-flex-basis", barWidth value maxValue)]
            ] []
        , if displayLabels then label value else text ""
        ]

    -- HTML for a label
    label value =
      div [ class "label count" ]
        [ text <| toString value
        ]

    -- The rows of the chart
    rows =
      List.map bar values

    -- Possibly display a blank row at the top
    rowsWithBlank =
      if displayExtraRow
      then (bar 0) :: rows
      else rows

  in
    div [ class "bars" ] rowsWithBlank


-- Calculate the width of a given bar as a proportion of the maximum
barWidth : Float -> Float -> String
barWidth value maxValue =
  toString (value / maxValue * 100) ++ "%"


-- HTML for a set of labels
labels : Bool -> String -> List String -> Html a
labels displayExtraLabel extraLabelText items =
  let
    -- A single label
    label className item =
      div [ class <| "label text " ++ className ] [ text item ]

    -- The rows of labels
    rows = 
      List.map (label "") items

    -- Possibly display an extra label at the top
    rowsWithBlank =
      if displayExtraLabel
      then (label "extra-row" extraLabelText) :: rows
      else rows

  in
    div [ class "labels" ] rowsWithBlank


-- HTML for a regular bar chart
barChart : BarChartOptions -> Html a
barChart { heading, className, items } =
  let
    maxValue =
      maxValueInList <| List.map snd items

  in
    div [ class <| "chart bar-chart " ++ className ]
      [ div [ class "heading" ] [ text heading ]
      , div [ class "chart-container" ] 
          [ (labels False "" <| List.map fst items)
          , (bars False True maxValue <| List.map snd items)
          ]
      ]


-- HTML for a 'double' bar chart
doubleBarChart : DoubleBarChartOptions -> Html a
doubleBarChart { className, formatPrefix, formatSuffix, totalLabelA, totalLabelB, items } =
  let
    -- The first set of values
    valuesA =
      List.map (\(_, i, _) -> i) items

    -- The second set of values
    valuesB =
      List.map (\(_, _, i) -> i) items

    -- The first total figure
    totalValueA =
      H.toFixed 2 <| List.sum valuesA

    -- The second total figure
    totalValueB =
      H.toFixed 2 <| List.sum valuesB

    combinedTotal =
      totalValueA + totalValueB

    -- The maximum value of either set of values
    maxValue =
      max (maxValueInList valuesA) (maxValueInList valuesB)

    valuesToStrings values =
      List.map (\v -> formatPrefix ++ (toString v) ++ formatSuffix) values

    -- HTML for one part of the 'totals' section under the chart
    totalFigure (figure, label) =
      div [ class "figure", style [("width", barWidth figure combinedTotal)] ]
        [ span [ class "text" ] [ text <| "Total " ++ (String.toLower label) ++ ": " ]
        , span [ class "fig" ] [ text <| formatPrefix ++ (toString figure) ++ formatSuffix ]
        ]

    -- HTML for the 'totals' section under the chart
    totalFiguresSection =
      div [ class "figures" ]
        [ totalFigure (totalValueA, totalLabelA)
        , totalFigure (totalValueB, totalLabelB)
        ]

  in
    div [ class <| "chart bar-chart-double " ++ className ]
      [ div [ class "chart-container" ]
          [ (bars True False maxValue valuesA)
          , (labels True totalLabelA <| valuesToStrings valuesA)
          , (labels True "" <| List.map (\(s, _, _) -> s) items)
          , (labels True totalLabelB <| valuesToStrings valuesB)
          , (bars True False maxValue valuesB)
          ]
      , totalFiguresSection
      ]


-- HTML for the football pitch chart
pitchChart : BarChartOptions -> Html a
pitchChart { heading, className, items } =
  let
    -- HTML for one of the circles on the chart
    circle item =
      let
        value =
          snd item

        radius =
          toString <| value / 3

        c =
          H.slugify <| fst item 

      in
        div
          [ class <| "circle " ++ c
          , style
              [ ("width", radius ++ "rem")
              , ("height", radius ++ "rem")
              ]
          ]
          [ span [ class "label" ]
              [ text <| toString value
              ]
          ]

    -- One circle per item
    circles =
      List.map circle items

  in
    div [ class <| "chart pitch-chart" ]
      [ div [ class "heading" ] [ text heading ]
      , div [ class "chart-container" ]
          [ img [ src "assets/newpitch.svg" ] []
          , div [ class "circles" ] circles
          ]
      ]


-- HTML for the world map chart
mapChart : MapChartOptions -> Html a
mapChart { heading, items } =
  let
    -- Get all the items for a given region
    itemsForRegion name =
      List.filter (\(r, _, _) -> r == name) items

    -- HTML for the circles overlaid on the map
    circles =
      let
        circle name count =
          div [ class <| "circle " ++ H.slugify name ]
            [ span [ class "label" ]
                [ text <| toString count
                ]
            ]

        count name =
          List.foldr (\(_, _, i) c -> i + c) 0 (itemsForRegion name)

        regionalCounts =
          List.map count regionNames

      in
        List.map2 circle regionNames regionalCounts

    -- HTML for the lists of values under the map
    region name =
      let
        drawLi (_, n, i) =
          li []
            [ span [ class "country" ] [ text n ]
            , span [ class "count" ] [ text <| toString i ]
            ]

        sort =
          List.reverse << List.sortBy (\(r, n, i) -> i)

      in
        div [ class <| "region " ++ (H.slugify name) ]
          [ span [ class "region-title" ] [ text name ]
          , ul [] (List.map drawLi <| sort <| itemsForRegion name)
          ]

    -- The regions the map is split into
    regionNames =
      [ "North America", "South America", "Africa", "Europe", "Asia & Oceania" ]

  in
    div [ class <| "chart map-chart" ]
      [ div [ class "heading" ] [ text heading ]
      , div [ class "chart-container" ]
          ((img [ src "assets/map.svg" ] []) :: circles)
      , div [ class "regions" ]
          (List.map region regionNames)
      ]
