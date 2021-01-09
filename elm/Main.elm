module Main exposing (main)

import Api.Generated exposing (Secret, Widget(..), widgetDecoder)
import Browser
import Html exposing (Html, div, h1, h2, p, pre, text)
import Json.Decode as D


type Model
    = SecretModel Secret
    | ErrorModel String


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html msg
view model =
    div []
        [ text "<🔑>"
        , widgetView model
        , text "</🔑>"
        ]


widgetView : Model -> Html msg
widgetView model =
    case model of
        ErrorModel errorMsg ->
            errorView errorMsg

        SecretModel secret ->
            secretView secret


errorView : String -> Html msg
errorView errorMsg =
    pre [] [ text "Widget Error: ", text errorMsg ]


secretView : Secret -> Html msg
secretView secret =
    div []
        [ h2 [] [ text secret.payload ]
        ]


showReview : Maybe String -> Html msg
showReview maybeReview =
    case maybeReview of
        Just review ->
            text ("Your book review: " ++ review)

        Nothing ->
            text "You have not reviewed this book"


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : D.Value -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags
    , Cmd.none
    )


initialModel : D.Value -> Model
initialModel flags =
    case D.decodeValue widgetDecoder flags of
        Ok widget ->
            widgetFlagToModel widget

        Err error ->
            ErrorModel (D.errorToString error)


widgetFlagToModel : Widget -> Model
widgetFlagToModel widget =
    case widget of
        SecretWidget book ->
            SecretModel book