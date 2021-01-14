module Widget.SecretCreator exposing (..)

import Api.Generated exposing (InputSecret, Link, Secret)
import Api.Http exposing (postSecretAction)
import Crypto.Hash
import Crypto.Strings as Strings
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Material.Button as Button
import Material.TextField as TextField
import Material.Typography as Typography
import Random exposing (Seed, initialSeed)
import Task
import Time exposing (Posix)


type alias Model =
    { payload : Maybe String
    , password : Maybe String
    , secret : Maybe Link
    , seed : Maybe Seed
    }


type FormField
    = Payload
    | Password


initialModel : Model
initialModel =
    { payload = Nothing
    , password = Nothing
    , secret = Nothing
    , seed = Nothing
    }


init : Model -> ( Model, Cmd Msg )
init model =
    ( model, Task.perform InitializeSeed Time.now )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = NoOp
    | InitializeSeed Posix
    | SetPayload String
    | SetPassword String
    | SubmitForm
    | Response (Result Http.Error Link)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InitializeSeed posix ->
            ( { model | seed = Just (initialSeed <| Time.posixToMillis posix) }
            , Cmd.none
            )

        SetPayload payload ->
            ( { model | payload = Just payload }, Task.perform InitializeSeed Time.now )

        SetPassword password ->
            ( { model | password = Just password }, Task.perform InitializeSeed Time.now )

        SubmitForm ->
            case ( model.password, model.payload, model.seed ) of
                ( Just passphrase, Just plaintext, Just seed ) ->
                    let
                        hashedPassword =
                            Crypto.Hash.sha512 passphrase

                        ( ciphertext, seed1 ) =
                            case Strings.encrypt seed passphrase plaintext of
                                Err msg1 ->
                                    ( "Error: " ++ msg1, seed )

                                Ok textAndSeed ->
                                    textAndSeed
                    in
                    ( { model | secret = Nothing }
                    , postSecretAction (InputSecret ciphertext hashedPassword) Response
                    )

                ( _, _, _ ) ->
                    ( model, Cmd.none )

        Response (Ok response) ->
            ( { model | secret = Just response }, Cmd.none )

        Response (Err error) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.secret of
        Nothing ->
            div [ class "container h-100" ]
                [ div [ class "row h-50 justify-content-center align-items-center" ]
                    [ div [ class "text-center" ]
                        [ h4 [ Typography.headline4 ] [ text "🔑 Create a new secret:" ]
                        , materialTextField (Maybe.withDefault "" model.payload) "text" "Secret content goes here..." [] "face" True SetPayload
                        , materialTextField (Maybe.withDefault "" model.password) "text" "A word or phrase that's difficult to guess" [] "face" True SetPassword
                        , buttonView model
                        ]
                    ]
                ]

        Just secret ->
            let
                link =
                    secret.link
            in
            div [ class "container h-100" ]
                [ div [ class "row h-50 justify-content-center align-items-center" ]
                    [ div [ class "text-center" ]
                        [ h4 [ Typography.headline4 ] [ text "🔑 Link to the secret:" ]
                        , a [ Typography.button, href link ] [ text link ]
                        ]
                    ]
                ]


buttonView : Model -> Html Msg
buttonView model =
    case ( model.password, model.payload, model.seed ) of
        ( Just _, Just _, Just _ ) ->
            Button.raised (Button.config |> Button.setOnClick SubmitForm) "Create link"

        _ ->
            Button.raised (Button.config |> Button.setDisabled True) "Create link"


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
