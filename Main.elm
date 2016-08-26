port module TransferWindow exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList, id, src)

import Json.Decode as Json exposing ((:=), Value)
import Time exposing (..)
import String exposing (toLower)

import Types exposing (..)

import Countdown.Countdown as Countdown
import TeamPage.TeamPage as TeamPage
import OverviewPage.OverviewPage as OverviewPage


-- Wire up the program
main =
  App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



{-------
  MODEL
--------}

-- Our model of global state
type alias Model =
  { countdownTimer : Countdown.Model      -- The current state of the countdown timer
  , currentPage : Page                    -- The currently displayed page
  , teams : List TeamData                 -- Team data from the spreadsheet
  , topTen : List TopTenData              -- 'Top ten' data from the spreadsheet
  , activeTeamPageData : Maybe TeamData   -- Data for the currently active team page 
  , overviewPageData : OverviewPage.Model -- Data for the overview page
  }


type Msg
  = ChangePage Page
  | NewTeams (List Json.Value)
  | NewTransfers (List Json.Value)
  | NewTopTen (List Json.Value)
  | Tick Time
  | TeamPageMsg TeamPage.Msg
  | OverviewPageMsg OverviewPage.Msg


-- Start off with 'blank' team data
initialTeamData =
  [ TeamData "" [] "Arsenal" []
  , TeamData "" [] "Bournemouth" []
  , TeamData "" [] "Burnley" []
  , TeamData "" [] "Chelsea" []
  , TeamData "" [] "Crystal Palace" []
  , TeamData "" [] "Everton" []
  , TeamData "" [] "Hull City" []
  , TeamData "" [] "Leicester" []
  , TeamData "" [] "Liverpool" []
  , TeamData "" [] "Man City" []
  , TeamData "" [] "Man United" []
  , TeamData "" [] "Middlesbrough" []
  , TeamData "" [] "Southampton" []
  , TeamData "" [] "Stoke City" []
  , TeamData "" [] "Sunderland" []
  , TeamData "" [] "Swansea" []
  , TeamData "" [] "Tottenham" []
  , TeamData "" [] "Watford" []
  , TeamData "" [] "West Brom" []
  , TeamData "" [] "West Ham" []
  ]


-- Instantiate our model
init : (Model, Cmd Msg)
init =
  (
    { countdownTimer = Countdown.init
    , currentPage = Overview
    , teams = initialTeamData
    , topTen = []
    , activeTeamPageData = TeamPage.init
    , overviewPageData = OverviewPage.init
    }
  , Cmd.none)



{---------------
  SUBSCRIPTIONS
----------------}

-- Listen for incoming team data
port teamJson : (List Json.Value -> msg) -> Sub msg


-- Listen for incoming transfers data
port transfersJson : (List Json.Value -> msg) -> Sub msg


-- Listen for incoming transfers data
port topTenJson : (List Json.Value -> msg) -> Sub msg


-- When data from a subscription comes in, fire the relevant message
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ teamJson NewTeams
    , transfersJson NewTransfers
    , topTenJson NewTopTen
    , Time.every Time.second Tick
    ]



{--------
  UPDATE
---------}

-- Update our model according to the given message
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- Change to a new page
    ChangePage newPage ->
      let
        activeTeamPage =
          case newPage of
            Overview ->
              Nothing

            Team teamName ->
              getTeamData teamName model.teams

      in
        ({ model
          | currentPage = newPage
          , activeTeamPageData = TeamPage.update (TeamPage.UpdateTeamData activeTeamPage) model.activeTeamPageData
          }, Cmd.none)

    -- Decode and store new teams data from the spreadsheet
    NewTeams teamData ->
      let
        updatedTeamData =
          updateTeamData teamData model.teams

      in
        ({ model
          | teams = updatedTeamData
          , overviewPageData = OverviewPage.update (OverviewPage.UpdateTeamData updatedTeamData) model.overviewPageData
          }, Cmd.none)

    -- Decode and store new transfers data from the spreadsheet
    NewTransfers transfersData ->
      let
        updatedTransfersData =
          updateTransfersData transfersData model.teams

      in
        ({ model
          | teams = updatedTransfersData
          , overviewPageData = OverviewPage.update (OverviewPage.UpdateTeamData updatedTransfersData) model.overviewPageData
          }, Cmd.none)

    -- Decode and store new 'top ten' data from the spreadsheet
    NewTopTen topTenData ->
      let
        updatedTopTenData =
          updateTopTenData topTenData

      in
        ({ model
          | topTen = updatedTopTenData
          , overviewPageData = OverviewPage.update (OverviewPage.UpdateTopTenData updatedTopTenData) model.overviewPageData
          }, Cmd.none)
    
    -- Update the countdown timer on each tick
    Tick newTime ->
      ({ model | countdownTimer = Countdown.update newTime model.countdownTimer }, Cmd.none)

    -- Wrap internal TeamPage messages
    TeamPageMsg teamMsg ->
      ({ model | activeTeamPageData = TeamPage.update teamMsg model.activeTeamPageData }, Cmd.none)

    -- Wrap internal OverviewPage messages
    OverviewPageMsg overviewMsg ->
      ({ model | overviewPageData = OverviewPage.update overviewMsg model.overviewPageData }, Cmd.none)



{---------------
  JSON DECODING
----------------}

-- Decode a given JSON object into key-value string pairs
decodeToStringPairs : Json.Value -> Maybe (List (String, String))
decodeToStringPairs = 
  Result.toMaybe << Json.decodeValue (Json.keyValuePairs Json.string)


-- In a list of key-value pairs, find the value for a given key, if it exists
getValue : List (String, String) -> String -> String
getValue pairs key =
  let
    match =
      List.head <| List.filter (\t -> fst t == key) pairs

  in
    case match of
      Just value ->
        snd value

      Nothing ->
        ""


-- Find the TeamData for a given team name, if it exists
getTeamData : String -> List TeamData -> Maybe TeamData
getTeamData teamName teams =
  List.head <| List.filter (\t -> teamName == t.name) teams


-- Decode JSON from the spreadsheet into records representing the data for each team page
decodeTeamData : List Json.Value -> List TeamData
decodeTeamData teamData =
  let
    -- Run the decoder on an item from the spreadsheet
    convertJsonToRecord : Json.Value -> Maybe TeamData
    convertJsonToRecord datum =
      case (decodeToStringPairs datum) of
        -- If we successfully decode into key-value pairs, construct a TeamData record
        Just pairs ->
          Just (
            { author = getValue pairs "Author"
            , copy = copySections pairs
            , name = getValue pairs "Club"
            , transfers = []
            }
          )

        -- Otherwise return Nothing
        Nothing ->
          Nothing

    -- Construct CopySectionData records from the decoded data and label them
    copySections : List (String, String) -> List (String, CopySectionData)
    copySections pairs =
      [ ("need"
        , { copy = getValue pairs "What do they need blurb"
          , heading = "What do they need?"
          , image = getValue pairs "What do they need image"
          , player = getValue pairs "What do they need"
          }
        )
      , ("go-for"
        , { copy = getValue pairs "Who they will go for blurb"
          , heading = "Who will they go for?"
          , image = getValue pairs "Who they will go for image"
          , player = getValue pairs "Who they will go for"
          }
        )
      , ("key-signing"
        , { copy = getValue pairs "Key signing blurb"
          , heading = "Key signing"
          , image = getValue pairs "Key signing image"
          , player = getValue pairs "Key signing"
          }
        )
      , ("key-departures"
        , { copy = getValue pairs "Key departure blurb"
          , heading = "Key departure"
          , image = getValue pairs "Key departure image"
          , player = getValue pairs "Key departure"
          }
        )
      ]

  in
    List.filterMap convertJsonToRecord teamData


-- Map new team data to the existing team records in the model
updateTeamData : List Json.Value -> List TeamData -> List TeamData
updateTeamData newTeamData existingTeams = 
  let
    newTeams =
      decodeTeamData newTeamData

    -- Match a new team to the correct existing team, and update the relevant fields
    update existingTeam =
      case (getTeamData existingTeam.name newTeams) of
        Just newTeam ->
          { existingTeam | author = newTeam.author, copy = newTeam.copy }

        Nothing ->
          existingTeam

  in
    List.map update existingTeams


-- Decode JSON from the spreadsheet into records representing the transfers data for each team
decodeTransfersData : List Json.Value -> List (String, TransferData)
decodeTransfersData transfersData =
  let
    -- Run the decoder on an item from the spreadsheet
    convertJsonToRecord : Json.Value -> Maybe (String, TransferData)
    convertJsonToRecord datum =
      case (decodeToStringPairs datum) of
        -- If we successfully decode into key-value pairs, construct a TransferData record
        Just pairs ->
          Just (getValue pairs "Club Name", buildRecord pairs)

        -- Otherwise return Nothing
        Nothing ->
          Nothing

    -- Lift a string into a Maybe
    maybeEmpty : String -> Maybe String
    maybeEmpty str =
      if (str == "-" || str == "") then Nothing else Just str

    -- Format the data as we want it
    buildRecord : List (String, String) -> TransferData
    buildRecord pairs =
      { player = getValue pairs "Player Name"
      , transferType = if String.toLower (getValue pairs "Transfer Type") == "in" then In else Out
      , location = getValue pairs "From/To"
      , feeStr = getValue pairs "Fee"
      , feeNum = (Result.toMaybe << String.toFloat) <| getValue pairs "Fee as Number"
      , signedFrom = maybeEmpty (getValue pairs "Where were they signed from?")
      , nationality = maybeEmpty (getValue pairs "Nationality")
      , region = maybeEmpty (getValue pairs "Region/Continent")
      , age = (Result.toMaybe << String.toInt) <| getValue pairs "Age"
      , position = maybeEmpty (getValue pairs "Position")
      }

  in 
    List.filterMap convertJsonToRecord transfersData


-- Map new transfers data to the existing team records in the model
updateTransfersData : List Json.Value -> List TeamData -> List TeamData
updateTransfersData transfersData teams =
  let
    transfers =
      decodeTransfersData transfersData

    -- Filter the transfers to only those for the given team
    transfersForTeam team =
      List.map snd << List.filter (\(teamName, transfer) -> teamName == team.name)

    -- Update the relevant fields
    update team =
      { team | transfers = transfersForTeam team transfers }

  in
    List.map update teams


-- Decode JSON from the spreadsheet into records repredenting the 'top ten transfers' data
decodeTopTenData : List Json.Value -> List TopTenData
decodeTopTenData topTenData =
  let
    -- Run the decoder on an item from the spreadsheet
    convertJsonToRecord : Json.Value -> Maybe TopTenData
    convertJsonToRecord datum =
      case (decodeToStringPairs datum) of
        Just pairs ->
          Just (buildRecord pairs)

        Nothing ->
          Nothing

    -- Structure the data as we want it
    buildRecord : List (String, String) -> TopTenData
    buildRecord pairs =
      { fee = getValue pairs "Fee"
      , from =  getValue pairs "From"
      , image = getValue pairs "Image URL"
      , player = getValue pairs "Player"
      , to = getValue pairs "To"
      }

  in
    List.filterMap convertJsonToRecord topTenData


-- Map new 'top ten transfers' data to the existing model
updateTopTenData : List Json.Value -> List TopTenData
updateTopTenData topTenData =
  decodeTopTenData topTenData



{------
  VIEW
-------}

view : Model -> Html Msg
view model =
  div []
  [ header []
    [ div [ class "wrapper" ]
      [ h2 [] [ text "Premier League transfer window" ]
      , h3 [ class "countdown" ]
        [ span [ class "label" ] [ text "Countdown " ]
        , (App.map Tick <| Countdown.view model.countdownTimer)
        ]
      ]
    ]
  , div [ class "app-wrapper" ]
    [ menu model.teams model.currentPage
    , div [ class "page-wrapper" ] [ pageToDisplay model ]
    ]
  ]


-- Display the correct page
pageToDisplay : Model -> Html Msg
pageToDisplay model =
  case model.currentPage of
    Overview ->
      App.map OverviewPageMsg <| OverviewPage.view model.overviewPageData

    Team teamName ->
      App.map TeamPageMsg <| TeamPage.view model.activeTeamPageData



{------
  MENU
-------}

-- Location of badge SVGs
badgeUrl : String
badgeUrl =
  "http://nuk-tnl-editorial-prod-staticassets.s3.amazonaws.com/2016/bespoke/0721-TransferWindow/badges/"


-- HTML for the menu
menu : List TeamData -> Page -> Html Msg
menu teams currentPage =
  let
    teamNames =
      List.map .name teams

  in
    nav [ id "menu", class "mobile" ]
      [ ul []
        ((menuItem currentPage Overview) :: (List.map (menuItem currentPage << Team) teamNames))
      ]


-- HTML for a menu item, including onClick events
menuItem : Page -> Page -> Html Msg
menuItem currentPage page =
  let
    isActive =
      page == currentPage

    liContent teamName =
      [ span [] [ text teamName ]
      , div [ class "image" ]
        [ img [ src <| badgeUrl ++ teamName ++ ".svg" ] []
        ]
      ]

  in
    case page of
      Overview ->
        li [ onClick (ChangePage Overview), classList [("active", isActive)] ] (liContent "Overview")

      Team teamName as teamPage ->
        li [ onClick (ChangePage teamPage), classList [("active", isActive)] ] (liContent teamName)

