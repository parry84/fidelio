module Widget.SecretCreator exposing (..)

import Api.Generated exposing (InputSecret, Link, Secret)
import Api.Http exposing (postSecretAction)
import Crypto.Hash
import Crypto.Strings as Strings
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Material.Button as Button
import Material.TextField as TextField
import Material.TextField.Icon as TextFieldIcon
import Material.Typography as Typography
import Random exposing (Seed, initialSeed)
import Rumkin exposing (Strength(..), getStats, parseCommonList, parseFrequencyList)
import Task
import Time exposing (Posix)
import Widget.Helper exposing (layout)


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
    layout
        (case model.secret of
            Nothing ->
                creatorForm model

            Just secret ->
                linkView secret
        )


creatorForm : Model -> List (Html Msg)
creatorForm model =
    [ h4 [ Typography.headline4 ] [ text "🔑 Create a new secret:" ]
    , materialTextField (Maybe.withDefault "" model.payload) "text" "Secret content goes here..." [] "face" True SetPayload
    , materialTextField (Maybe.withDefault "" model.password) "password" "A word or phrase that's difficult to guess" [] "face" True SetPassword
    , div [] [text ""]
    ]
        ++ passwordStrength model.password
        ++ [ buttonView model
           ]


linkView : Link -> List (Html Msg)
linkView secretLink =
    let
        link =
            secretLink.link
    in
    [ h4 [ Typography.headline4 ] [ text "🔑 Link to the secret:" ]
    , a [ Typography.button, href link ] [ text link ]
    ]


passwordStrength : Maybe String -> List (Html Msg)
passwordStrength maybeBassword =
    case maybeBassword of
        Just password ->
            let
                stats =
                    Rumkin.getStats password

                strength =
                    stats.strength
            in
            [ div [ Typography.caption ]
                [ text "Strength: "
                , case strength of
                    VeryWeak ->
                        text "very weak"

                    Weak ->
                        text "weak"

                    Reasonable ->
                        text "reasonable"

                    Strong ->
                        text "strong"

                    VeryStrong ->
                        text "very strong"
                ]
            ]

        Nothing ->
            []


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
