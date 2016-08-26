module OverviewPage.TopTenSection exposing (topTenSection)

import Html exposing (..)
import Html.Attributes exposing (class, style)

import Types exposing (TopTenData)


-- Given data, output the 'top ten' section
topTenSection : List TopTenData -> Html a
topTenSection topTen =
  div [ class "top-ten" ]
    [ h1 []
      [ text "Top ten transfer fees"
      ]
    , ul []
      (List.map topTenItem topTen)
    ]


-- HTML for an individual item in the 'top ten' section
topTenItem : TopTenData -> Html a
topTenItem data =
  li []
    [ div [ class "player-name" ] [ text data.player ]
    , div [ class "player-image roundel", style [("backgroundImage", "url('" ++ data.image ++ "')")] ] []
    , div [ class "from" ]
        [ span [] [ text "From: " ]
        , text data.from
        ]
    , div [ class "to" ]
        [ span [] [ text "To: " ]
        , text data.to
        ]
    , div [ class "fee" ]
        [ h1 [] [ text data.fee ]
        ]
    ]
