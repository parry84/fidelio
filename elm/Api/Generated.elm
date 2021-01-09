module Api.Generated exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type Widget 
    = SecretWidget Secret


widgetEncoder : Widget -> Json.Encode.Value
widgetEncoder a =
    case a of
        SecretWidget b ->
            secretEncoder b


widgetDecoder : Json.Decode.Decoder Widget
widgetDecoder =
    Json.Decode.map SecretWidget secretDecoder


type alias Secret  =
    { id : String, payload : String }


secretEncoder : Secret -> Json.Encode.Value
secretEncoder a =
    Json.Encode.object [ ("id" , Json.Encode.string a.id)
    , ("payload" , Json.Encode.string a.payload) ]


secretDecoder : Json.Decode.Decoder Secret
secretDecoder =
    Json.Decode.succeed Secret |>
    Json.Decode.Pipeline.required "id" Json.Decode.string |>
    Json.Decode.Pipeline.required "payload" Json.Decode.string