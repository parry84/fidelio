module Widget.SecretCreator exposing (..)

import Api.Generated exposing (Secret)
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
    , secret : Maybe Secret
    , seed : Seed
    , seedReady : Bool
    }

type FormField
    = Payload
    | Password


initialModel : Model
initialModel =
    { payload = Nothing
    , password = Nothing
    , secret = Nothing
    , seed = initialSeed 0
    , seedReady = False
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
    | SeedInitialized ()
    | SetPayload String
    | SetPassword String
    | SubmitForm
    | Response (Result Http.Error Secret)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InitializeSeed posix ->
            ( { model | seed = initialSeed <| Time.posixToMillis posix }
            , Task.perform SeedInitialized <| Task.succeed ()
            )

        SeedInitialized _ ->
            ( { model | seedReady = True }, Cmd.none )

        SetPayload payload ->
            ( { model | payload = Just payload }, Cmd.none )

        SetPassword password ->
            ( { model | password = Just password }, Cmd.none )

        SubmitForm ->
            case (model.password, model.payload) of
                (Just passphrase, Just plaintext) -> 
                    let
                        ( ciphertext, seed ) =
                            case Strings.encrypt model.seed passphrase plaintext of
                                Err msg1 ->
                                    ( "Error: " ++ msg1, model.seed)

                                Ok textAndSeed ->
                                    textAndSeed
                    in
                        ( { model | secret = Nothing }
                        , postSecretAction (Secret "" ciphertext) Response
                        )
                (_, _) -> 
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
                , button
                    ([] |> appendIf model.seedReady (onClick SubmitForm))
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

appendIf : Bool -> a -> List a -> List a
appendIf flag value list =
    if flag == True then
        list ++ [ value ]
    else
        list