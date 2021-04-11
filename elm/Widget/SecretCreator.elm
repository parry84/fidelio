port module Widget.SecretCreator exposing (..)

import Api.Generated exposing (InputSecret, Lifetime(..), Link, PayloadType(..))
import Api.Http exposing (postSecretAction)
import Base64
import Crypto.Hash
import Crypto.Strings as Strings
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Material.Button as Button
import Material.Card as Card
import Material.HelperText as HelperText
import Material.Select as Select
import Material.Select.Item as SelectItem exposing (SelectItem)
import Material.TextField as TextField
import Material.TextField.Icon as TextFieldIcon
import Material.Typography as Typography
import Random exposing (Seed, initialSeed)
import Rumkin exposing (Strength(..))
import Task
import Time exposing (Posix)
import Widget.Helper exposing (layout)


port copyLink : () -> Cmd msg


type alias Plaintext =
    { payloadType : PayloadType
    , payload : String
    }


type alias Model =
    { payload : Maybe Plaintext
    , password : Maybe String
    , lifetime : Maybe Lifetime
    , secret : Maybe Link
    , seed : Maybe Seed
    , passwordVisible : Bool
    , hover : Bool
    , preview : Maybe String
    }


type Msg
    = NoOp
    | InitializeSeed Posix
    | SetPayload String
    | SetPassword String
    | SetLifetime (Maybe Lifetime)
    | SubmitForm
    | Response (Result Http.Error Link)
    | SetPasswordVisibility
    | CopyToClipboard
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | GotPreview String
    | GotContent (Maybe String)


initialModel : Model
initialModel =
    { payload = Nothing
    , password = Nothing
    , lifetime = Just Lifetime1h
    , secret = Nothing
    , seed = Nothing
    , passwordVisible = True
    , hover = False
    , preview = Nothing
    }


init : Model -> ( Model, Cmd Msg )
init model =
    ( model, Task.perform InitializeSeed Time.now )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InitializeSeed posix ->
            ( { model | seed = Just (initialSeed <| Time.posixToMillis posix) }
            , Cmd.none
            )

        SetPayload plaintext ->
            ( { model | payload = Just { payloadType = Message, payload = plaintext } }, Cmd.none )

        SetPassword password ->
            ( { model | password = Just password }, Task.perform InitializeSeed Time.now )

        SetLifetime lifetime ->
            ( { model | lifetime = lifetime }, Cmd.none )

        SubmitForm ->
            case ( model.password, model.payload, model.seed ) of
                ( Just passphrase, Just plaintext, Just seed ) ->
                    let
                        lifetime =
                            Maybe.withDefault Lifetime1h model.lifetime

                        hashedPassword =
                            Crypto.Hash.sha512 passphrase

                        ( ciphertext, _ ) =
                            case Strings.encrypt seed passphrase plaintext.payload of
                                Err msg1 ->
                                    ( "Error: " ++ msg1, seed )

                                Ok textAndSeed ->
                                    textAndSeed
                    in
                    ( { model | secret = Nothing }
                    , postSecretAction (InputSecret plaintext.payloadType ciphertext hashedPassword lifetime) Response
                    )

                ( _, _, _ ) ->
                    ( model, Cmd.none )

        Response (Ok response) ->
            ( { model | secret = Just response }, Cmd.none )

        Response (Err error) ->
            ( model, Cmd.none )

        SetPasswordVisibility ->
            ( { model | passwordVisible = not model.passwordVisible }, Cmd.none )

        CopyToClipboard ->
            ( model, copyLink () )

        DragEnter ->
            ( { model | hover = True }
            , Cmd.none
            )

        DragLeave ->
            ( { model | hover = False }
            , Cmd.none
            )

        GotFiles file _ ->
            ( { model | hover = False }
            , Cmd.batch <| [ Task.perform GotPreview (File.toUrl file), Task.perform (GotContent << Base64.fromBytes) (File.toBytes file) ]
            )

        GotPreview url ->
            ( { model | preview = Just url }
            , Cmd.none
            )

        GotContent (Just content) ->
            ( { model | payload = Just { payloadType = Image, payload = content } }
            , Cmd.none
            )

        GotContent Nothing ->
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
    , secretField model
    , passwordField model
    , lifetimeSelect model
    , buttonView model
    ]


linkView : Link -> List (Html Msg)
linkView secretLink =
    let
        link =
            secretLink.link
    in
    [ h4 [ Typography.headline4 ] [ text "🔑 Link to the secret:" ]
    , pre [ id "link", Typography.button ] [ text link ]
    , Button.raised (Button.config |> Button.setOnClick CopyToClipboard) "Copy to clipboard"
    ]


buttonView : Model -> Html Msg
buttonView model =
    div textFieldContainer
        [ case ( model.password, model.payload, model.seed ) of
            ( Just _, Just _, Just _ ) ->
                Button.raised (Button.config |> Button.setOnClick SubmitForm) "Create link"

            _ ->
                Button.raised (Button.config |> Button.setDisabled True) "Create link"
        ]


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
                |> TextField.setPlaceholder (Just "A word or phrase that's hard to guess")
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
            :: passwordStrength model.password
        )


secretField : Model -> Html Msg
secretField model =
    case model.payload of
        Nothing ->
            secretFieldText ""

        Just payload ->
            case payload.payloadType of
                Message ->
                    secretFieldText payload.payload

                Image ->
                    Card.card Card.config
                        { blocks =
                            [ Card.block <|
                                div
                                    [ class "d-flex justify-content-center"
                                    , style "padding" "30px"
                                    ]
                                    [ viewPreview (Maybe.withDefault "" model.preview) ]
                            ]
                        , actions = Nothing
                        }

                _ ->
                    text "Unsupported format"


secretFieldText : String -> Html Msg
secretFieldText message =
    div dragContainer
        [ div textFieldContainer
            [ TextField.filled
                (TextField.config
                    |> TextField.setType (Just "text")
                    |> TextField.setAttributes [ style "width" "100%", class "material-text-field" ]
                    |> TextField.setPlaceholder (Just "Secret content goes here...")
                    |> TextField.setValue (Just message)
                    |> TextField.setMaxLength (Just 1000)
                    |> TextField.setRequired True
                    |> TextField.setOnInput SetPayload
                    |> TextField.setValid (not (String.isEmpty message))
                )
            , helperText
            ]
        ]


dragContainer : List (Attribute Msg)
dragContainer =
    [ hijackOn "dragenter" (D.succeed DragEnter)
    , hijackOn "dragover" (D.succeed DragEnter)
    , hijackOn "dragleave" (D.succeed DragLeave)
    , hijackOn "drop" dropDecoder
    ]


viewPreview : String -> Html msg
viewPreview url =
    div
        [ style "width" "60px"
        , style "height" "60px"
        , style "background-image" ("url('" ++ url ++ "')")
        , style "background-position" "center"
        , style "background-repeat" "no-repeat"
        , style "background-size" "contain"
        ]
        []


helperText : Html msg
helperText =
    HelperText.helperLine []
        [ HelperText.helperText
            (HelperText.config |> HelperText.setPersistent True)
            ""
        , HelperText.characterCounter []
        ]


passwordStrength : Maybe String -> List (Html Msg)
passwordStrength maybeBassword =
    case maybeBassword of
        Just password ->
            let
                stats =
                    Rumkin.getStats password

                strength =
                    case stats.strength of
                        VeryWeak ->
                            "very weak"

                        Weak ->
                            "weak"

                        Reasonable ->
                            "reasonable"

                        Strong ->
                            "strong"

                        VeryStrong ->
                            "very strong"
            in
            [ HelperText.helperLine []
                [ HelperText.helperText
                    (HelperText.config |> HelperText.setPersistent True)
                    ("Strength: " ++ strength)
                ]
            ]

        Nothing ->
            []


lifetimeSelect : Model -> Html Msg
lifetimeSelect model =
    div textFieldContainer
        [ Select.filled
            (Select.config
                |> Select.setAttributes []
                |> Select.setLabel (Just "Lifetime")
                |> Select.setSelected (Just model.lifetime)
                |> Select.setOnChange SetLifetime
            )
            firstItem
            remainingItems
        ]


firstItem : SelectItem (Maybe Lifetime) msg
firstItem =
    SelectItem.selectItem
        (SelectItem.config { value = Just Lifetime5m })
        [ text "5 mins" ]


remainingItems : List (SelectItem (Maybe Lifetime) msg)
remainingItems =
    [ SelectItem.selectItem
        (SelectItem.config { value = Just Lifetime10m })
        [ text "10 mins" ]
    , SelectItem.selectItem
        (SelectItem.config { value = Just Lifetime15m })
        [ text "15 mins" ]
    , SelectItem.selectItem
        (SelectItem.config { value = Just Lifetime1h })
        [ text "1 hour" ]
    , SelectItem.selectItem
        (SelectItem.config { value = Just Lifetime4h })
        [ text "4 hours" ]
    , SelectItem.selectItem
        (SelectItem.config { value = Just Lifetime12h })
        [ text "12 hours" ]
    , SelectItem.selectItem
        (SelectItem.config { value = Just Lifetime1d })
        [ text "1 day" ]
    , SelectItem.selectItem
        (SelectItem.config { value = Just Lifetime3d })
        [ text "3 days" ]
    , SelectItem.selectItem
        (SelectItem.config { value = Just Lifetime7d })
        [ text "7 days" ]
    ]


textFieldContainer : List (Html.Attribute msg)
textFieldContainer =
    [ class "text-field-container"
    , style "min-width" "200px"
    , style "margin-top" "20px"
    ]


dropDecoder : D.Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files" ] (D.oneOrMore GotFiles File.decoder)


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (D.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )
