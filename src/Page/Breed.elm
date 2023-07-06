module Page.Breed exposing (..)

import Helpers.Message as Message
import Html exposing (Html, div, h1, img, p, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Decode exposing (Decoder, field, list)
import Page.Breeds exposing (Breed, SubBreed, breedToString, subBreedToString)


type alias Model =
    { breedPics : BreedPicturesRequest
    }


type BreedPicturesRequest
    = Loading
    | Failure Http.Error
    | Success (List ImageLink)


type ImageLink
    = ImageLink String


imageLinkToString : ImageLink -> String
imageLinkToString (ImageLink str) =
    str


stringToImageLink : String -> ImageLink
stringToImageLink str =
    ImageLink str


init : Breed -> Maybe SubBreed -> ( Model, Cmd Msg )
init breed maybeSubBreed =
    ( Model Loading, getBreedImages breed maybeSubBreed )


type Msg
    = GotPictures (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPictures result ->
            case result of
                Ok imageStrs ->
                    let
                        imageLinks =
                            List.map stringToImageLink imageStrs
                    in
                    ( { model | breedPics = Success imageLinks }, Cmd.none )

                Err err ->
                    ( { model | breedPics = Failure err }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Dog Breeds" ]
        , p [] [ text "Click a breed (or sub-breed) to view some puppy pics!" ]
        , case model.breedPics of
            Loading ->
                div [] [ text Message.loading ]

            Failure _ ->
                div [] [ text Message.failureToFetch ]

            Success pictures ->
                div [] <| List.map renderImages pictures
        ]


renderImages : ImageLink -> Html Msg
renderImages pic =
    let
        imageStr =
            imageLinkToString pic
    in
    img [ src imageStr ] []


getBreedImages : Breed -> Maybe SubBreed -> Cmd Msg
getBreedImages breed maybeSubBreed =
    let
        base =
            "https://images.dog.ceo/breeds/" ++ breedToString breed

        imagePath =
            "/images"

        url =
            case maybeSubBreed of
                Nothing ->
                    base ++ imagePath

                Just subBreed ->
                    base ++ subBreedToString subBreed ++ imagePath
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotPictures messageDecoder
        }


messageDecoder : Decoder (List String)
messageDecoder =
    field "message" (list Decode.string)
