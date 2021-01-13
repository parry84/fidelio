module Widget.SecretViewer exposing (..)

import Api.Generated exposing (InputPassword, OutputSecret, Secret, SecretViewerFlags)
import Api.Http exposing (getSecretAction)
import Crypto.Strings as Strings
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Material.Button as Button
import Material.TextField as TextField
import Material.Typography as Typography
import Random exposing (Seed, initialSeed)


type alias Model =
    { secretId : String
    , chipertext : Maybe String
    , password : Maybe String
    , plaintext : Maybe String
    , response : Maybe String
    }


init : Model -> ( Model, Cmd msg )
init model =
    ( model, Cmd.none )


initialModel : SecretViewerFlags -> Model
initialModel secretViewerFlags =
    { secretId = secretViewerFlags.secretId
    , chipertext = Nothing
    , password = Nothing
    , plaintext = Nothing
    , response = Nothing
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = NoOp
    | SetPassword String
    | SubmitForm
    | Response (Result Http.Error OutputSecret)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetPassword password ->
            ( { model | password = Just password }, Cmd.none )

        SubmitForm ->
            case model.password of
                Just passphrase ->
                    ( model
                    , getSecretAction (InputPassword model.secretId passphrase) Response
                    )

                _ ->
                    ( model, Cmd.none )

        Response (Ok response) ->
            case ( model.password, model.chipertext ) of
                ( Just passphrase, _ ) ->
                    let
                        chipertext =
                            response.payload

                        plaintext =
                            case Strings.decrypt passphrase chipertext of
                                Err msg1 ->
                                    "Error: " ++ msg1

                                Ok textAndSeed ->
                                    textAndSeed
                    in
                    ( { model | plaintext = Just plaintext }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Response (Err error) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.plaintext of
        Nothing ->
            div [ class "container h-100" ]
                [ div [ class "row h-50 justify-content-center align-items-center" ]
                    [ div [ class "text-center" ]
                        [ h4 [ Typography.headline4 ] [ text "🔑 This message requires a passphrase:" ]
                        , materialTextField (Maybe.withDefault "" model.password) "text" "Enter the passphrase here" [] "face" True SetPassword
                        , buttonView model
                        ]
                    ]
                ]

        Just plaintext ->
            div [ class "container h-100" ]
                [ div [ class "row h-50 justify-content-center align-items-center" ]
                    [ div [ class "text-center" ]
                        [ h4 [ Typography.headline4 ] [ text "🔑 The secret:" ]
                        , pre [ Typography.button ] [ text plaintext ]
                        ]
                    ]
                ]


buttonView : Model -> Html Msg
buttonView model =
    case model.password of
        Just _ ->
            Button.raised (Button.config |> Button.setOnClick SubmitForm) "View secret"

        _ ->
            Button.raised (Button.config |> Button.setDisabled True) "View secret"


materialTextField : String -> String -> String -> List (Attribute Msg) -> String -> Bool -> (String -> Msg) -> Html Msg
materialTextField str setType placeholder arr icon isValid updateFunction =
    TextField.filled
        (TextField.config
            |> TextField.setType (Just setType)
            |> TextField.setAttributes ([ style "width" "100%", class "material-text-field" ] ++ arr)
            |> TextField.setPlaceholder (Just placeholder)
            |> TextField.setValue (Just str)
            |> TextField.setRequired True
            |> TextField.setOnInput updateFunction
            |> TextField.setValid (not (String.isEmpty str))
        )
