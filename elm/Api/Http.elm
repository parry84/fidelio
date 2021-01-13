module Api.Http exposing (..)

import Api.Generated exposing (..)
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
    InputSecret
    -> (Result Http.Error Link -> msg)
    -> Cmd msg
postSecretAction secret msg =
    ihpRequest
        { method = "POST"
        , headers = []
        , url = "/CreateSecret"
        , body = Http.jsonBody <| inputSecretEncoder secret
        , expect = Http.expectJson msg linkDecoder
        }


getSecretAction :
    InputPassword
    -> (Result Http.Error OutputSecret -> msg)
    -> Cmd msg
getSecretAction password msg =
    ihpRequest
        { method = "POST"
        , headers = []
        , url = "/Get"
        , body = Http.jsonBody <| inputPasswordEncoder password
        , expect = Http.expectJson msg outputSecretDecoder
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
