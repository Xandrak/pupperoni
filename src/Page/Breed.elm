module Page.Breed exposing (BreedPicturesRequest, Model, Msg, init, update, view)

import Helpers.Message as Message
import Html exposing (Html, div, h1, img, p, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Decode exposing (Decoder, field, list)
import Json.Encode as Encode
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
    | Failure
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
                        imageLinks : List ImageLink
                        imageLinks =
                            List.map stringToImageLink imageStrs

                        encodedImageLinks : Encode.Value
                        encodedImageLinks =
                            encodeImageLinks imageLinks breed maybeSubBreed
                    in
                    ( { model | breedPics = Success imageLinks }, Ports.setStorage encodedImageLinks )

                Err _ ->
                    ( { model | breedPics = Failure }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Dog Breeds" ]
        , p [] [ text "Here you go, enjoy!" ]
        , case model.breedPics of
            Loading ->
                div [] [ text Message.loading ]

            Failure ->
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
        imageStr : String
        imageStr =
            imageLinkToString pic
    in
    img [ src imageStr ] []



-- HTTP


getBreedImages : Breed -> Maybe SubBreed -> Cmd Msg
getBreedImages breed maybeSubBreed =
    let
        base : String
        base =
            "https://dog.ceo/api/breed/" ++ breedToString breed

        imagePath : String
        imagePath =
            "/images"

        url : String
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
        imageLinkStrs : List String
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
