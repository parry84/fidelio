module Main exposing (main)

import Api.Generated
    exposing
        ( Secret
        , Widget(..)
        , secretDecoder
        , widgetDecoder
        )
import Browser
import Html exposing (..)
import Json.Decode as D
import Widget.SecretCreator
import Widget.SecretViewer


type Model
    = SecretModel Widget.SecretViewer.Model
    | SecretCreatorModel Widget.SecretCreator.Model
    | ErrorModel String


type Msg
    = GotSecretMsg Widget.SecretViewer.Msg
    | GotSecretCreatorMsg Widget.SecretCreator.Msg
    | WidgetErrorMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotSecretMsg subMsg, SecretModel secret ) ->
            Widget.SecretViewer.update subMsg secret
                |> updateWith SecretModel GotSecretMsg model

        ( GotSecretCreatorMsg subMsg, SecretCreatorModel subModel ) ->
            Widget.SecretCreator.update subMsg subModel
                |> updateWith SecretCreatorModel GotSecretCreatorMsg model

        ( WidgetErrorMsg, ErrorModel _ ) ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateWith :
    (subModel -> Model)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )


subscriptions : Model -> Sub Msg
subscriptions parentModel =
    case parentModel of
        SecretModel secret ->
            Sub.map GotSecretMsg
                (Widget.SecretViewer.subscriptions secret)

        SecretCreatorModel subModel ->
            Sub.map GotSecretCreatorMsg
                (Widget.SecretCreator.subscriptions subModel)

        ErrorModel err ->
            Sub.none


view : Model -> Html Msg
view model =
    case model of
        ErrorModel errorMsg ->
            errorView errorMsg

        SecretCreatorModel subModel ->
            Html.map GotSecretCreatorMsg (Widget.SecretCreator.view subModel)

        SecretModel secret ->
            Html.map GotSecretMsg (Widget.SecretViewer.view secret)


errorView : String -> Html msg
errorView errorMsg =
    pre [] [ text "Widget Error: ", text errorMsg ]


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
        SecretViewerWidget secretViewerFlags ->
            SecretModel (Widget.SecretViewer.initialModel secretViewerFlags)

        SecretCreatorWidget ->
            SecretCreatorModel Widget.SecretCreator.initialModel
