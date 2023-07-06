module Api.Breeds exposing (BreedRequest(..), getAllBreeds)

import Http
import Json.Decode as Decode exposing (Decoder, field, keyValuePairs, list)

type BreedRequest
    = Failure Http.Error
    | Loading
    | Success (List ( String, List String ))


getAllBreeds : (Result Http.Error (List (String, List String)) -> msg) -> Cmd msg
getAllBreeds expectedMsg =
    Http.get
        { url = "https://dog.ceo/api/breeds/list/all"
        , expect = Http.expectJson expectedMsg messageDecoder
        }


messageDecoder : Decoder (List ( String, List String ))
messageDecoder =
    field "message" (keyValuePairs (list Decode.string))
