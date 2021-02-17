module Api.Generated.Lifetime exposing (..)

import Api.Generated
import Json.Decode
import Json.Encode


encoder : Api.Generated.Lifetime -> Json.Encode.Value
encoder a =
    case a of
        Api.Generated.Lifetime5m ->
            Json.Encode.string "Lifetime5m"
        
        Api.Generated.Lifetime10m ->
            Json.Encode.string "Lifetime10m"
        
        Api.Generated.Lifetime15m ->
            Json.Encode.string "Lifetime15m"
        
        Api.Generated.Lifetime1h ->
            Json.Encode.string "Lifetime1h"
        
        Api.Generated.Lifetime4h ->
            Json.Encode.string "Lifetime4h"
        
        Api.Generated.Lifetime12h ->
            Json.Encode.string "Lifetime12h"
        
        Api.Generated.Lifetime1d ->
            Json.Encode.string "Lifetime1d"
        
        Api.Generated.Lifetime3d ->
            Json.Encode.string "Lifetime3d"
        
        Api.Generated.Lifetime7d ->
            Json.Encode.string "Lifetime7d"


decoder : Json.Decode.Decoder Api.Generated.Lifetime
decoder =
    Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "Lifetime5m" ->
            Json.Decode.succeed Api.Generated.Lifetime5m
        
        "Lifetime10m" ->
            Json.Decode.succeed Api.Generated.Lifetime10m
        
        "Lifetime15m" ->
            Json.Decode.succeed Api.Generated.Lifetime15m
        
        "Lifetime1h" ->
            Json.Decode.succeed Api.Generated.Lifetime1h
        
        "Lifetime4h" ->
            Json.Decode.succeed Api.Generated.Lifetime4h
        
        "Lifetime12h" ->
            Json.Decode.succeed Api.Generated.Lifetime12h
        
        "Lifetime1d" ->
            Json.Decode.succeed Api.Generated.Lifetime1d
        
        "Lifetime3d" ->
            Json.Decode.succeed Api.Generated.Lifetime3d
        
        "Lifetime7d" ->
            Json.Decode.succeed Api.Generated.Lifetime7d
        
        _ ->
            Json.Decode.fail "No matching constructor")