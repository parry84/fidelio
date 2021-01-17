module Widget.Helper exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


layout : List (Html msg) -> Html msg
layout inner =
    div [ class "container h-100" ]
        [ div [ class "row h-50 justify-content-center align-items-center" ]
            [ div [ class "text-center" ]
                inner
            ]
        ]
