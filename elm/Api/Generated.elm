module Api.Generated exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


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
    { payload : String, password : String }


inputSecretEncoder : InputSecret -> Json.Encode.Value
inputSecretEncoder a =
    Json.Encode.object
        [ ( "payload", Json.Encode.string a.payload )
        , ( "password", Json.Encode.string a.password )
        ]


inputSecretDecoder : Json.Decode.Decoder InputSecret
inputSecretDecoder =
    Json.Decode.succeed InputSecret
        |> Json.Decode.Pipeline.required "payload" Json.Decode.string
        |> Json.Decode.Pipeline.required "password" Json.Decode.string


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
