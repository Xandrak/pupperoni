module Main exposing (Model, Msg, main)

import Browser
import Browser.Navigation as Nav
import Http
import Json.Decode exposing (Decoder, field, keyValuePairs, list, string)
import Url



-- MAIN


type BreedRequest
    = Failure Http.Error
    | Loading
    | Success (List ( String, List String ))


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , dogBreeds : BreedRequest
    }



-- add flags/check for session storage here


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url Loading
    , getAllBreeds
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotBreeds (Result Http.Error (List ( String, List String )))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        GotBreeds result ->
            case result of
                Ok breeds ->
                    let
                        sortedBreeds =
                            List.sort breeds
                    in
                    -- persist in session storage here as well
                    ( { model | dogBreeds = Success sortedBreeds }, Cmd.none )

                Err err ->
                    ( { model | dogBreeds = Failure err }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view _ =
    { title = "Puppy Power!"
    , body = body
    }



getAllBreeds : Cmd Msg
getAllBreeds =
    Http.get
        { url = "https://dog.ceo/api/breeds/list/all"
        , expect = Http.expectJson GotBreeds messageDecoder
        }


messageDecoder : Decoder (List ( String, List String ))
messageDecoder =
    field "message" (keyValuePairs (list string))

