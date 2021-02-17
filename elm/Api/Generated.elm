module Api.Generated exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type Lifetime
    = Lifetime5m
    | Lifetime10m
    | Lifetime15m
    | Lifetime1h
    | Lifetime4h
    | Lifetime12h
    | Lifetime1d
    | Lifetime3d
    | Lifetime7d


encoder : Lifetime -> Json.Encode.Value
encoder a =
    case a of
        Lifetime5m ->
            Json.Encode.string "5m"

        Lifetime10m ->
            Json.Encode.string "10m"

        Lifetime15m ->
            Json.Encode.string "15m"

        Lifetime1h ->
            Json.Encode.string "1h"

        Lifetime4h ->
            Json.Encode.string "4h"

        Lifetime12h ->
            Json.Encode.string "12h"

        Lifetime1d ->
            Json.Encode.string "1d"

        Lifetime3d ->
            Json.Encode.string "3d"

        Lifetime7d ->
            Json.Encode.string "7d"


decoder : Json.Decode.Decoder Lifetime
decoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\a ->
                case a of
                    "5m" ->
                        Json.Decode.succeed Lifetime5m

                    "10m" ->
                        Json.Decode.succeed Lifetime10m

                    "15m" ->
                        Json.Decode.succeed Lifetime15m

                    "1h" ->
                        Json.Decode.succeed Lifetime1h

                    "4h" ->
                        Json.Decode.succeed Lifetime4h

                    "12h" ->
                        Json.Decode.succeed Lifetime12h

                    "1d" ->
                        Json.Decode.succeed Lifetime1d

                    "3d" ->
                        Json.Decode.succeed Lifetime3d

                    "7d" ->
                        Json.Decode.succeed Lifetime7d

                    _ ->
                        Json.Decode.fail "No matching constructor"
            )


type alias SecretViewerFlags =
    { secretId : String }


secretViewerFlagsEncoder : SecretViewerFlags -> Json.Encode.Value
secretViewerFlagsEncoder a =
    Json.Encode.object [ ( "secretId", Json.Encode.string a.secretId ) ]


secretViewerFlagsDecoder : Json.Decode.Decoder SecretViewerFlags
secretViewerFlagsDecoder =
    Json.Decode.succeed SecretViewerFlags
        |> Json.Decode.Pipeline.required "secretId" Json.Decode.string


type alias Secret =
    { id : String, payload : String, password : String }


secretEncoder : Secret -> Json.Encode.Value
secretEncoder a =
    Json.Encode.object
        [ ( "id", Json.Encode.string a.id )
        , ( "payload", Json.Encode.string a.payload )
        , ( "password", Json.Encode.string a.password )
        ]


secretDecoder : Json.Decode.Decoder Secret
secretDecoder =
    Json.Decode.succeed Secret
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "payload" Json.Decode.string
        |> Json.Decode.Pipeline.required "password" Json.Decode.string


type alias Link =
    { link : String }


linkEncoder : Link -> Json.Encode.Value
linkEncoder a =
    Json.Encode.object [ ( "link", Json.Encode.string a.link ) ]


linkDecoder : Json.Decode.Decoder Link
linkDecoder =
    Json.Decode.succeed Link
        |> Json.Decode.Pipeline.required "link" Json.Decode.string


type alias InputSecret =
    { payload : String, password : String, lifetime : Lifetime }


inputSecretEncoder : InputSecret -> Json.Encode.Value
inputSecretEncoder a =
    Json.Encode.object
        [ ( "payload", Json.Encode.string a.payload )
        , ( "password", Json.Encode.string a.password )
        , ( "lifetime", encoder a.lifetime )
        ]


inputSecretDecoder : Json.Decode.Decoder InputSecret
inputSecretDecoder =
    Json.Decode.succeed InputSecret
        |> Json.Decode.Pipeline.required "payload" Json.Decode.string
        |> Json.Decode.Pipeline.required "password" Json.Decode.string
        |> Json.Decode.Pipeline.required "lifetime" decoder


type alias InputPassword =
    { id : String, password : String }


inputPasswordEncoder : InputPassword -> Json.Encode.Value
inputPasswordEncoder a =
    Json.Encode.object
        [ ( "id", Json.Encode.string a.id )
        , ( "password", Json.Encode.string a.password )
        ]


inputPasswordDecoder : Json.Decode.Decoder InputPassword
inputPasswordDecoder =
    Json.Decode.succeed InputPassword
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "password" Json.Decode.string


type alias OutputSecret =
    { payload : String }


outputSecretEncoder : OutputSecret -> Json.Encode.Value
outputSecretEncoder a =
    Json.Encode.object [ ( "payload", Json.Encode.string a.payload ) ]


outputSecretDecoder : Json.Decode.Decoder OutputSecret
outputSecretDecoder =
    Json.Decode.succeed OutputSecret
        |> Json.Decode.Pipeline.required "payload" Json.Decode.string


type Widget
    = SecretViewerWidget SecretViewerFlags
    | SecretCreatorWidget


widgetEncoder : Widget -> Json.Encode.Value
widgetEncoder a =
    case a of
        SecretViewerWidget b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "SecretViewerWidget" )
                , ( "contents", secretViewerFlagsEncoder b )
                ]

        SecretCreatorWidget ->
            Json.Encode.object [ ( "tag", Json.Encode.string "SecretCreatorWidget" ) ]


widgetDecoder : Json.Decode.Decoder Widget
widgetDecoder =
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\a ->
                case a of
                    "SecretViewerWidget" ->
                        Json.Decode.succeed SecretViewerWidget
                            |> Json.Decode.Pipeline.required "contents" secretViewerFlagsDecoder

                    "SecretCreatorWidget" ->
                        Json.Decode.succeed SecretCreatorWidget

                    _ ->
                        Json.Decode.fail "No matching constructor"
            )
