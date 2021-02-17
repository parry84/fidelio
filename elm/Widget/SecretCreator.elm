port module Widget.SecretCreator exposing (..)

import Api.Generated exposing (InputSecret, Link, Secret, Lifetime)
import Api.Http exposing (postSecretAction)
import Crypto.Hash
import Crypto.Strings as Strings
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Material.Button as Button
import Material.HelperText as HelperText
import Material.Select as Select
import Material.Select.Item as SelectItem exposing (SelectItem)
import Material.TextField as TextField
import Material.TextField.Icon as TextFieldIcon
import Material.Typography as Typography
import Random exposing (Seed, initialSeed)
import Rumkin exposing (Strength(..), getStats, parseCommonList, parseFrequencyList)
import Task
import Time exposing (Posix)
import Widget.Helper exposing (layout)
import Api.Generated exposing (Lifetime(..))


type alias Model =
    { payload : Maybe String
    , password : Maybe String
    , lifetime : Maybe Lifetime
    , secret : Maybe Link
    , seed : Maybe Seed
    , passwordVisible : Bool
    }


type FormField
    = Payload
    | Password


port copyLink : () -> Cmd msg


initialModel : Model
initialModel =
    { payload = Nothing
    , password = Nothing
    , lifetime = Nothing
    , secret = Nothing
    , seed = Nothing
    , passwordVisible = True
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
    | SetLifetime (Maybe Lifetime)
    | SubmitForm
    | Response (Result Http.Error Link)
    | SetPasswordVisibility
    | CopyToClipboard


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
            ( { model | payload = Just payload }, Cmd.none )

        SetPassword password ->
            ( { model | password = Just password }, Task.perform InitializeSeed Time.now )

        SetLifetime lifetime ->
            ( { model | lifetime = lifetime }, Cmd.none)

        SubmitForm ->
            case ( model.password, model.payload, model.seed ) of
                ( Just passphrase, Just plaintext, Just seed ) ->
                    let
                        lifetime = Maybe.withDefault Lifetime1h model.lifetime
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
                    , postSecretAction (InputSecret ciphertext hashedPassword lifetime) Response
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
    case ( model.password, model.payload, model.seed ) of
        ( Just _, Just _, Just _ ) ->
            Button.raised (Button.config |> Button.setOnClick SubmitForm) "Create link"

        _ ->
            Button.raised (Button.config |> Button.setDisabled True) "Create link"


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
        ([ TextField.filled
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
         ]
            ++ passwordStrength model.password
        )


secretField : Model -> Html Msg
secretField model =
    let
        value =
            Maybe.withDefault "" model.payload
    in
    div textFieldContainer
        [ TextField.filled
            (TextField.config
                |> TextField.setType (Just "text")
                |> TextField.setAttributes [ style "width" "100%", class "material-text-field" ]
                |> TextField.setPlaceholder (Just "Secret content goes here...")
                |> TextField.setValue (Just value)
                |> TextField.setMaxLength (Just 1000)
                |> TextField.setRequired True
                |> TextField.setOnInput SetPayload
                |> TextField.setValid (not (String.isEmpty value))
            )
        , helperText
        ]


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
lifetimeSelect
 model =
    div textFieldContainer [
    Select.filled
        (Select.config
            |> Select.setAttributes [ style "width" "100%" ]
            |> Select.setLabel (Just "Lifetime")
            |> Select.setSelected (Just model.lifetime)
            |> Select.setOnChange SetLifetime
        )
        firstItem
        remainingItems ]

firstItem : SelectItem (Maybe a) msg
firstItem =
    SelectItem.selectItem
        (SelectItem.config { value = Nothing })
        [ text "" ]

remainingItems : List (SelectItem (Maybe Lifetime) msg)
remainingItems =
    [ SelectItem.selectItem
        (SelectItem.config { value = Just Lifetime5m })
        [ text "5 mins" ]
    , SelectItem.selectItem
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
    ]
