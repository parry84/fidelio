module Api.Lifetime exposing (..)

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
            Json.Encode.string "Lifetime5m"
        
        Lifetime10m ->
            Json.Encode.string "Lifetime10m"
        
        Lifetime15m ->
            Json.Encode.string "Lifetime15m"
        
        Lifetime1h ->
            Json.Encode.string "Lifetime1h"
        
        Lifetime4h ->
            Json.Encode.string "Lifetime4h"
        
        Lifetime12h ->
            Json.Encode.string "Lifetime12h"
        
        Lifetime1d ->
            Json.Encode.string "Lifetime1d"
        
        Lifetime3d ->
            Json.Encode.string "Lifetime3d"
        
        Lifetime7d ->
            Json.Encode.string "Lifetime7d"


decoder : Lifetime -> Json.Encode.Value
decoder a =
    case a of
        Lifetime5m ->
            Json.Encode.string "Lifetime5m"
        
        Lifetime10m ->
            Json.Encode.string "Lifetime10m"
        
        Lifetime15m ->
            Json.Encode.string "Lifetime15m"
        
        Lifetime1h ->
            Json.Encode.string "Lifetime1h"
        
        Lifetime4h ->
            Json.Encode.string "Lifetime4h"
        
        Lifetime12h ->
            Json.Encode.string "Lifetime12h"
        
        Lifetime1d ->
            Json.Encode.string "Lifetime1d"
        
        Lifetime3d ->
            Json.Encode.string "Lifetime3d"
        
        Lifetime7d ->
            Json.Encode.string "Lifetime7d"