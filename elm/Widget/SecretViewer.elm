port module Widget.SecretViewer exposing (..)

import Api.Generated exposing (InputPassword, OutputSecret, PayloadType(..), SecretViewerFlags)
import Api.Http exposing (getSecretAction)
import Crypto.Hash
import Crypto.Strings as Strings
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Material.Button as Button
import Material.Card as Card
import Material.HelperText as HelperText
import Material.TextField as TextField
import Material.TextField.Icon as TextFieldIcon
import Material.Typography as Typography
import Widget.Helper exposing (layout)


type alias Plaintext =
    { payloadType : PayloadType
    , payload : String
    }


type alias Model =
    { secretId : String
    , chipertext : Maybe String
    , password : Maybe String
    , plaintext : Maybe Plaintext
    , passwordVisible : Bool
    , errors : List Http.Error
    }


port copySecret : () -> Cmd msg


init : Model -> ( Model, Cmd msg )
init model =
    ( model, Cmd.none )


initialModel : SecretViewerFlags -> Model
initialModel secretViewerFlags =
    { secretId = secretViewerFlags.secretId
    , chipertext = Nothing
    , password = Nothing
    , plaintext = Nothing
    , passwordVisible = True
    , errors = []
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
    | CopyToClipboard


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
                    ( { model | plaintext = Just { payloadType = response.payloadType, payload = plaintext } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Response (Err error) ->
            ( { model | errors = error :: model.errors }, Cmd.none )

        SetPasswordVisibility ->
            ( { model | passwordVisible = not model.passwordVisible }, Cmd.none )

        CopyToClipboard ->
            ( model, copySecret () )


view : Model -> Html Msg
view model =
    layout
        (case model.plaintext of
            Nothing ->
                decryptView model

            Just plaintext ->
                secretView plaintext
        )


decryptView : Model -> List (Html Msg)
decryptView model =
    [ h4 [ Typography.headline4 ] [ text "🔑 This message requires a passphrase:" ]
    , passwordField model
    , buttonView model
    , p [ Typography.body2 ] [ text "pay attention: we will show it only once." ]
    ]


secretView : Plaintext -> List (Html Msg)
secretView plaintext =
    [ h4 [ Typography.headline4 ] [ text "🔑 The secret:" ]
    , case plaintext.payloadType of
        Message ->
            pre [ id "secret", Typography.button ] [ text plaintext.payload ]

        Image ->
            Card.card Card.config
                { blocks =
                    [ Card.block <|
                        div
                            [ class "d-flex justify-content-center"
                            , style "padding" "30px"
                            ]
                            [ viewPreview plaintext.payload ]
                    ]
                , actions = Nothing
                }

        _ ->
            text "Unsupported format"
    , Button.raised (Button.config |> Button.setOnClick CopyToClipboard) "Copy to clipboard"
    , p [ Typography.body2 ] [ text "pay attention: we will show it only once." ]
    ]


viewPreview : String -> Html msg
viewPreview url =
    img
        [ style "width" "60px"
        , style "height" "60px"
        , src ("data:image/png;base64," ++ url)
        ]
        []


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
    div textFieldContainer
        (TextField.filled
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
            :: passwordError model.errors
        )


passwordError : List Http.Error -> List (Html Msg)
passwordError errors =
    case errors of
        [] ->
            []

        _ ->
            [ HelperText.helperLine []
                [ HelperText.helperText
                    (HelperText.config |> HelperText.setPersistent True)
                    "Wrong password"
                ]
            ]


textFieldContainer : List (Html.Attribute msg)
textFieldContainer =
    [ class "text-field-container"
    , style "min-width" "200px"
    , style "margin-top" "20px"
    ]
