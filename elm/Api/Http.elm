module Api.Http exposing (..)

import Api.Generated exposing (Link, Secret, linkDecoder, secretDecoder, secretEncoder)
import Http
import Json.Decode as D


getSecretsAction :
    String
    -> (Result Http.Error (List Secret) -> msg)
    -> Cmd msg
getSecretsAction searchTerm msg =
    ihpRequest
        { method = "GET"
        , headers = []
        , url = "/Secrets?searchTerm=" ++ searchTerm
        , body = Http.emptyBody
        , expect = Http.expectJson msg (D.list secretDecoder)
        }


postSecretAction :
    Secret
    -> (Result Http.Error Link -> msg)
    -> Cmd msg
postSecretAction secret msg =
    ihpRequest
        { method = "POST"
        , headers = []
        , url = "/CreateSecret"
        , body = Http.jsonBody <| secretEncoder secret
        , expect = Http.expectJson msg linkDecoder
        }


ihpRequest :
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect msg
    }
    -> Cmd msg
ihpRequest { method, headers, url, body, expect } =
    Http.request
        { method = method
        , headers =
            [ Http.header "Accept" "application/json" ] ++ headers
        , url = url
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }
