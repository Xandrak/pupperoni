module Page.Breed exposing (Model, Msg, init, update, view)

import Helpers.Message as Message
import Html exposing (Html, div, h1, img, p, sub, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Decode exposing (Decoder, field, list)
import Json.Encode as Encode exposing (encode)
import Page.Breeds exposing (Breed, SubBreed, breedToString, subBreedToString)
import Ports



-- MODEL


type alias Model =
    { breedPics : BreedPicturesRequest
    }


init : Breed -> Maybe SubBreed -> ( Model, Cmd Msg )
init breed maybeSubBreed =
    ( Model Loading, getBreedImages breed maybeSubBreed )



-- TYPES


type BreedPicturesRequest
    = Loading
    | Failure Http.Error
    | Success (List ImageLink)


type ImageLink
    = ImageLink String


type Msg
    = GotPictures Breed (Maybe SubBreed) (Result Http.Error (List String))



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPictures breed maybeSubBreed result ->
            case result of
                Ok imageStrs ->
                    let
                        imageLinks =
                            List.map stringToImageLink imageStrs

                        encodedImageLinks =
                            encodeImageLinks imageLinks breed maybeSubBreed
                    in
                    ( { model | breedPics = Success imageLinks }, Ports.setStorage encodedImageLinks )

                Err err ->
                    ( { model | breedPics = Failure err }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Dog Breeds" ]
        , p [] [ text "Here you go, enjoy!" ]
        , case model.breedPics of
            Loading ->
                div [] [ text Message.loading ]

            Failure _ ->
                div [] [ text Message.failureToFetch ]

            Success pictures ->
                div [] <| List.map renderImages pictures
        ]



-- HELPERS


imageLinkToString : ImageLink -> String
imageLinkToString (ImageLink str) =
    str


stringToImageLink : String -> ImageLink
stringToImageLink str =
    ImageLink str


renderImages : ImageLink -> Html Msg
renderImages pic =
    let
        imageStr =
            imageLinkToString pic
    in
    img [ src imageStr ] []



-- HTTP


getBreedImages : Breed -> Maybe SubBreed -> Cmd Msg
getBreedImages breed maybeSubBreed =
    let
        base =
            "https://dog.ceo/api/breed/" ++ breedToString breed

        imagePath =
            "/images"

        url =
            case maybeSubBreed of
                Nothing ->
                    base ++ imagePath

                Just subBreed ->
                    base ++ "/" ++ subBreedToString subBreed ++ imagePath
    in
    Http.get
        { url = url
        , expect = Http.expectJson (GotPictures breed maybeSubBreed) messageDecoder
        }


messageDecoder : Decoder (List String)
messageDecoder =
    field "message" (list Decode.string)


encodeImageLinks : List ImageLink -> Breed -> Maybe SubBreed -> Encode.Value
encodeImageLinks imageLinks breed maybeSubBreed =
    let
        imageLinkStrs =
            List.map imageLinkToString imageLinks
    in
    case maybeSubBreed of
        Nothing ->
            Encode.object
                [ ( breedToString breed, Encode.list Encode.string imageLinkStrs )
                ]

        Just subBreed ->
            Encode.object [ ( subBreedToString subBreed, Encode.list Encode.string imageLinkStrs ) ]
