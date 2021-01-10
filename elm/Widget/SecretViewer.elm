module Widget.SecretViewer exposing (..)

import Api.Generated exposing (Secret)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Crypto.Strings as Strings
import Random exposing (Seed, initialSeed)
import Http


type alias Model =
    { secret : Secret
    , password : String
    , payload : Maybe String
    , response : Maybe String
    }


init : Model -> ( Model, Cmd msg )
init model =
    ( model, Cmd.none )

initialModel : Secret -> Model
initialModel secret =
    { secret = secret
    , password = ""
    , payload = Nothing
    , response = Nothing
    }

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = NoOp
    | SetPassword String
    | SubmitForm
    | Response (Result Http.Error (List Secret))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        SubmitForm ->
            let
                passphrase =
                    model.password

                ciphertext =
                    model.secret.payload

                plaintext =
                    case Strings.decrypt passphrase ciphertext of
                        Err msg1 ->
                            "Error: " ++ msg1

                        Ok textAndSeed ->
                            textAndSeed
            in
                ( { model | payload = Just plaintext }, Cmd.none )

        Response (Ok response) ->
            ( model, Cmd.none )

        Response (Err error) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.payload of
        Nothing ->
            div []
                [ h2 [] [ text "🔑 Insert the password to decrypt the secret:" ]
                , label []
                    [ text "Password"
                    , input
                        [ type_ "password"
                        , placeholder "Password"
                        , onInput SetPassword
                        , value model.password
                        ]
                        []
                    ]
                , button
                    [ onClick SubmitForm ]
                    [ text "View" ]
                ]

        Just plaintext ->
            div []
                [ h2 [] [ text "🔑 The secret:" ]
                , pre [] [ text plaintext]
                ]

