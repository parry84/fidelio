module Widget.SecretCreator exposing (..)

import Api.Generated exposing (Secret, Link)
import Api.Http exposing (postSecretAction)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Crypto.Strings as Strings
import Random exposing (Seed, initialSeed)
import Http
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
            case (model.password, model.payload, model.seed) of
                (Just passphrase, Just plaintext, Just seed) ->

                    let
                        ( ciphertext, seed1 ) =
                            case Strings.encrypt seed passphrase plaintext of
                                Err msg1 ->
                                    ( "Error: " ++ msg1, seed)

                                Ok textAndSeed ->
                                    textAndSeed
                    in
                        ( { model | secret = Nothing }
                        , postSecretAction (Secret "" ciphertext) Response
                        )
                (_, _, _) -> 
                    ( model, Cmd.none )

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
                            , value (Maybe.withDefault "" model.password)
                            ]
                            []
                        ]
                    ]
                , buttonView model.seed
                ]
        Just secret ->
            let
                link = secret.link
            in
                div []
                    [ h2 []
                        [ text "🔑 Link to the secret:" ]
                    , a [ href link ] [ text link ]
                    ]

buttonView : Maybe Seed -> Html Msg
buttonView seed = 
    case seed of
        Just _ ->
            button [ onClick SubmitForm ] [ text "Create link" ]
        Nothing ->
            button [] [ text "Create link" ]
