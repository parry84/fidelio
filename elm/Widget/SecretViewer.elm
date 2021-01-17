module Widget.SecretViewer exposing (..)

import Api.Generated exposing (InputPassword, OutputSecret, Secret, SecretViewerFlags)
import Api.Http exposing (getSecretAction)
import Crypto.Hash
import Crypto.Strings as Strings
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Material.Button as Button
import Material.TextField as TextField
import Material.TextField.Icon as TextFieldIcon
import Material.Typography as Typography
import Random exposing (Seed, initialSeed)
import Widget.Helper exposing (layout)


type alias Model =
    { secretId : String
    , chipertext : Maybe String
    , password : Maybe String
    , plaintext : Maybe String
    , response : Maybe String
    , passwordVisible : Bool
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
    , passwordVisible = True
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = NoOp
    | SetPassword String
    | SubmitForm
    | Response (Result Http.Error OutputSecret)
    | SetPasswordVisibility


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
                    , getSecretAction (InputPassword model.secretId (Crypto.Hash.sha512 passphrase)) Response
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

        SetPasswordVisibility ->
            ( { model | passwordVisible = not model.passwordVisible }, Cmd.none )


view : Model -> Html Msg
view model =
    layout
        (case model.plaintext of
            Nothing ->
                decryptView model

            Just secret ->
                secretView secret
        )


decryptView : Model -> List (Html Msg)
decryptView model =
    [ h4 [ Typography.headline4 ] [ text "🔑 This message requires a passphrase:" ]
    , passwordField model
    , buttonView model
    , p [ Typography.body2 ] [ text "pay attention: we will show it only once." ]
    ]


secretView : String -> List (Html Msg)
secretView plaintext =
    [ h4 [ Typography.headline4 ] [ text "🔑 The secret:" ]
    , pre [ Typography.button ] [ text plaintext ]
    , p [ Typography.body2 ] [ text "pay attention: we will show it only once." ]
    ]


buttonView : Model -> Html Msg
buttonView model =
    case model.password of
        Just _ ->
            Button.raised (Button.config |> Button.setOnClick SubmitForm) "View secret"

        _ ->
            Button.raised (Button.config |> Button.setDisabled True) "View secret"


passwordField : Model -> Html Msg
passwordField model =
    let
        textType =
            if model.passwordVisible then
                "text"

            else
                "password"

        icon =
            if model.passwordVisible then
                "visibility"

            else
                "visibility_off"

        value =
            Maybe.withDefault "" model.password
    in
    TextField.filled
        (TextField.config
            |> TextField.setType (Just textType)
            |> TextField.setAttributes [ style "width" "100%", class "material-text-field" ]
            |> TextField.setPlaceholder (Just "Enter the password here:")
            |> TextField.setValue (Just value)
            |> TextField.setRequired True
            |> TextField.setOnInput SetPassword
            |> TextField.setValid (not (String.isEmpty value))
            |> TextField.setTrailingIcon
                (Just
                    (TextFieldIcon.icon icon
                        |> TextFieldIcon.setOnInteraction SetPasswordVisibility
                    )
                )
        )
