module TeamPage.TeamPage exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (class, style)

import Types exposing (..)
import Helpers as H exposing (slugify)



{-------
  MODEL
--------}

type alias Model = Maybe TeamData


type Msg = UpdateTeamData (Maybe TeamData)


-- Initialise the page with no data
init : Model
init =
  Nothing



{--------
  UPDATE
---------}

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateTeamData data ->
      data



{------
  VIEW
-------}

-- Output either the data, or a holding message
view : Model -> Html Msg
view model =
  case model of
    Just data ->
      teamPage data

    Nothing ->
      div [] [ text "No data for this team yet" ]


-- HTML for a Team page
teamPage : TeamData -> Html Msg
teamPage team =
  let
    -- Slugified version of the team name
    teamSlug =
      H.slugify team.name

    -- HTML for the author's name
    authorSection : Html Msg
    authorSection =
      div [ class "author" ] [ text <| "Words: " ++ team.author ]

    -- HTML for the copy sections
    copySections : Html Msg
    copySections =
      let
        maybeCopy =
          case (List.head team.copy) of
            Just copy ->
              largeCopySection copy

            Nothing ->
              text ""

      in
        div [ class "copy-section-container" ]
          [ div [class "large" ] [ maybeCopy ]
          , div [ class "small" ]
              (List.map smallCopySection <| List.drop 1 team.copy)
          , authorSection
          ]

  in
    div [ class <| "page team team-" ++ teamSlug ]
      [ h1 [ class "team-name" ] [ text team.name ]
      , copySections
      , div [ class "table-section-container" ]
          [ tableSection In "Players in" "Total spent" team.transfers
          , tableSection Out "Players out" "Total received" team.transfers
          ]
      ]


-- HTML for a large copy section of a Team page
largeCopySection : (String, CopySectionData) -> Html Msg
largeCopySection (section, data) =
  div [ class <| "copy-section " ++ (H.slugify section) ]
    [ div [ class "text" ]
        [ div [ class "subheading" ] [ text data.heading ]
        , div [ class "player-name" ] [ text data.player ]
        , div [ class "copy" ] [ text data.copy ]
        ]
    , div [ class "player-image portrait", style [("backgroundImage", "url('" ++ (if data.image == "-" then "http://placehold.it/200" else data.image ) ++ "')")] ] []
    ]


-- HTML for a small copy section of a Team page
smallCopySection : (String, CopySectionData) -> Html Msg
smallCopySection (section, data) =
  div [ class <| "copy-section " ++ (H.slugify section) ]
    [ div [ class "player-image roundel", style [("backgroundImage", "url('" ++ data.image ++ "')")] ] []
    , div [ class "subheading" ] [ text data.heading ]
    , div [ class "player-name" ] [ text data.player ]
    , div [ class "copy" ] [ text data.copy ]
    ]


-- HTML for a total spent / received section of a Team page
tableSection : TransferType -> String -> String -> List TransferData -> Html Msg
tableSection transferType heading subheading transfers =
  let
    -- Only the In/Out transfers
    filteredTransfers =
      List.filter (\t -> t.transferType == transferType) transfers

    -- The total spent / received
    totalFig =
      List.foldr (\t sum -> sum + (Maybe.withDefault 0 t.feeNum)) 0 filteredTransfers

    -- Generate table rows
    tableRow transfer =
      tr []
        [ td [ class "player-name" ] [ text transfer.player ]
        , td [ class "to-from" ] [ text transfer.location ]
        , td [ class "fee" ] [ text transfer.feeStr ]
        ]

    -- Generate table headings
    tableHeading =
      tr []
        [ th [] [ text "Player" ]
        , th [] [ text "Club" ]
        , th [] [ text "Fee" ]
        ]

    -- Slugified version of the heading
    headingSlug =
      H.slugify heading

  in
    div [ class <| "table-section " ++ headingSlug ]
      [ div [ class "heading" ]
        [ h1 [] [ text heading ]
        ]
      , div [ class "subheading" ] [ text subheading ]
      , div [ class "figure" ]
        [ h1 [] [ text <| "£" ++ (toString totalFig) ++ "m" ]
        ]
      , div [ class "table-container" ]
        [ table [] (tableHeading :: (List.map tableRow filteredTransfers))
        ]
      ]
