module Api.Http exposing (..)

import Api.Generated exposing (Book, bookDecoder)
import Http
import Json.Decode as D


getSecretsAction :
    String
    -> (Result Http.Error (List Book) -> msg)
    -> Cmd msg
getSecretsAction searchTerm msg =
    ihpRequest
        { method = "GET"
        , headers = []
        , url = "/Secrets?searchTerm=" ++ searchTerm
        , body = Http.emptyBody
        , expect = Http.expectJson msg (D.list bookDecoder)
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