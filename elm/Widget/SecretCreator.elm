module Widget.SecretCreator exposing (..)

import Api.Generated exposing (Secret)
import Api.Http exposing (postSecretAction)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Crypto.Strings as Strings
import Random exposing (Seed, initialSeed)
import Http


type alias Model =
    { payload : String
    , password : String
    , secret : Maybe Secret
    }
type FormField
    = Payload
    | Password


initialModel : Model
initialModel =
    { payload = ""
    , password = ""
    , secret = Nothing
    }


init : Model -> ( Model, Cmd msg )
init model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = NoOp
    | SetPayload String
    | SetPassword String
    | SubmitForm
    | Response (Result Http.Error Secret)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetPayload email ->
            ( { model | payload = email }, Cmd.none )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        SubmitForm ->
            let
                passphrase =
                    model.password

                plaintext =
                    model.payload

                ( ciphertext, seed ) =
                    case Strings.encrypt (initialSeed 0) passphrase plaintext of
                        Err msg1 ->
                            ( "Error: " ++ msg1, (initialSeed 0))

                        Ok textAndSeed ->
                            textAndSeed
            in
                ( { model | secret = Nothing }
                , postSecretAction (Secret "" ciphertext) Response
                )

        Response (Ok response) ->
            ( { model | secret = Just response }, Cmd.none )

        Response (Err error) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.secret of
        Nothing ->
            div []
                [ h2 []
                    [ text "🔑 Create a new secret:" ]
                , p [] [
                        textarea 
                        [ cols 40
                        , rows 5
                        , placeholder "Secret content goes here..."
                        , onInput SetPayload
                        ]
                        []
                    ]
                , p [] [
                    label []
                        [ text "Password:"
                        , input
                            [ type_ "text"
                            , placeholder "A word or phrase that's difficult to guess"
                            , onInput SetPassword
                            , value model.password
                            ]
                            []
                        ]
                    ]
                , button
                    [ onClick SubmitForm ]
                    [ text "Create link" ]
                ]
        Just secret ->
            let
                link = "https://fidelio.ihpapp.com/ShowSecret?secretId=" ++ secret.id
            in
                div []
                    [ h2 []
                        [ text "🔑 Link to the secret:" ]
                    , a [ href link ] [ text link ]
                    ]
