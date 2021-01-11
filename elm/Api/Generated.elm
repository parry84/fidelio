module Api.Generated exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type Widget 
    = SecretViewerWidget Secret
    | SecretCreatorWidget 


widgetEncoder : Widget -> Json.Encode.Value
widgetEncoder a =
    case a of
        SecretViewerWidget b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "SecretViewerWidget")
            , ("contents" , secretEncoder b) ]
        
        SecretCreatorWidget ->
            Json.Encode.object [ ("tag" , Json.Encode.string "SecretCreatorWidget") ]


widgetDecoder : Json.Decode.Decoder Widget
widgetDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "SecretViewerWidget" ->
            Json.Decode.succeed SecretViewerWidget |>
            Json.Decode.Pipeline.required "contents" secretDecoder
        
        "SecretCreatorWidget" ->
            Json.Decode.succeed SecretCreatorWidget
        
        _ ->
            Json.Decode.fail "No matching constructor")


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


type alias Link  =
    { link : String }


linkEncoder : Link -> Json.Encode.Value
linkEncoder a =
    Json.Encode.object [("link" , Json.Encode.string a.link)]


linkDecoder : Json.Decode.Decoder Link
linkDecoder =
    Json.Decode.succeed Link |>
    Json.Decode.Pipeline.required "link" Json.Decode.string