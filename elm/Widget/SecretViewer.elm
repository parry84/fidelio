module Widget.SecretViewer exposing (..)

import Api.Generated exposing (Secret)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Crypto.Strings as Strings
import Random exposing (Seed, initialSeed)
import Http
import Material.Button as Button
import Material.TextField as TextField
import Material.Typography as Typography


type alias Model =
    { secret : Secret
    , password : Maybe String
    , payload : Maybe String
    , response : Maybe String
    }


init : Model -> ( Model, Cmd msg )
init model =
    ( model, Cmd.none )

initialModel : Secret -> Model
initialModel secret =
    { secret = secret
    , password = Nothing
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
            ( { model | password = Just password }, Cmd.none )

        SubmitForm ->
            case model.password of
                Just passphrase ->
                    let
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
                Nothing ->
                    ( model, Cmd.none )

        Response (Ok response) ->
            ( model, Cmd.none )

        Response (Err error) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.payload of
        Nothing ->
            div [ class "container h-100" ] [
                div [class "row h-50 justify-content-center align-items-center"] [
                    div [ class "text-center" ] 
                    [ h4 [ Typography.headline4 ] [ text "🔑 This message requires a passphrase:" ]
                    , materialTextField (Maybe.withDefault "" model.password) "text" "Enter the passphrase here" [] "face" (True) SetPassword
                    , buttonView model
                    ]]]

        Just plaintext ->
            div [ class "container h-100" ] [
                div [class "row h-50 justify-content-center align-items-center"] [
                    div [ class "text-center" ] 
                        [ h4 [ Typography.headline4 ] [ text "🔑 The secret:" ]
                        , pre [ Typography.button ] [ text plaintext ]
                        ]]]

buttonView : Model -> Html Msg
buttonView model = 
    case (model.password) of
        (Just _) ->
            Button.raised (Button.config |> Button.setOnClick SubmitForm) "View secret" 
        _ ->
            Button.raised (Button.config |> (Button.setDisabled True)) "View secret" 

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