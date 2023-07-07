module Page.Breed exposing (BreedPicturesRequest, Model, Msg, init, update, view)

import Helpers.Message as Message
import Html exposing (Html, button, div, h1, h3, img, p, text)
import Html.Attributes exposing (alt, disabled, src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, list)
import Json.Encode as Encode
import Page.Breeds exposing (Breed, SubBreed, breedToString, subBreedToString)
import Ports



-- MODEL


type alias Model =
    { breedPics : BreedPicturesRequest
    , breed : Breed
    , subBreed : Maybe SubBreed
    , currentPage : Int
    , itemsPerPage : Int
    }


init : Encode.Value -> Breed -> Maybe SubBreed -> ( Model, Cmd Msg )
init flags breed maybeSubBreed =
    let
        itemsPerPage : Int
        itemsPerPage =
            20

        currentPage : Int
        currentPage =
            1
    in
    case Decode.decodeValue (imageLinksDecoder breed maybeSubBreed) flags of
        Ok imageStrs ->
            let
                imageLinks : List ImageLink
                imageLinks =
                    List.map stringToImageLink imageStrs
            in
            ( Model (Success imageLinks) breed maybeSubBreed currentPage itemsPerPage, Cmd.none )

        Err _ ->
            ( Model Loading breed maybeSubBreed currentPage itemsPerPage, getBreedImages breed maybeSubBreed )



-- TYPES


type BreedPicturesRequest
    = Loading
    | Failure
    | Success (List ImageLink)


type ImageLink
    = ImageLink String


type Msg
    = GotPictures (Result Http.Error (List String))
    | PreviousPage
    | NextPage



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPictures result ->
            case result of
                Ok imageStrs ->
                    let
                        imageLinks : List ImageLink
                        imageLinks =
                            List.map stringToImageLink imageStrs

                        encodedImageLinks : Encode.Value
                        encodedImageLinks =
                            encodeImageLinks imageLinks model.breed model.subBreed
                    in
                    ( { model | breedPics = Success imageLinks }, Ports.setStorage encodedImageLinks )

                Err _ ->
                    ( { model | breedPics = Failure }, Cmd.none )

        PreviousPage ->
            ( { model | currentPage = model.currentPage - 1 }, Cmd.none )

        NextPage ->
            ( { model | currentPage = model.currentPage + 1 }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ h1 [] [ text "Dog Breeds" ]
        , p [] [ text "Here you go, enjoy!" ]
        , case model.breedPics of
            Loading ->
                div [] [ text Message.loading ]

            Failure ->
                div [] [ text Message.failureToFetch ]

            Success pictures ->
                let
                    numberOfPictures : Int
                    numberOfPictures =
                        List.length pictures
                in
                div []
                    [ h3 [] [ text <| "Total Pictures: " ++ String.fromInt numberOfPictures ]
                    , div
                        [ style "display" "grid"
                        , style "grid-template-columns" "repeat(auto-fill, minmax(200px, 3fr))"
                        , style "gap" "10px"
                        ]
                        (List.map renderImage (getPageImages model pictures))
                    , renderButtons model pictures
                    ]
        ]



-- HELPERS


imageLinkToString : ImageLink -> String
imageLinkToString (ImageLink str) =
    str


stringToImageLink : String -> ImageLink
stringToImageLink str =
    ImageLink str


renderImage : ImageLink -> Html Msg
renderImage pic =
    let
        imageStr : String
        imageStr =
            imageLinkToString pic
    in
    img
        [ style "max-width" "100%"
        , style "max-height" "100%"
        , src imageStr
        , alt "Image"
        ]
        []


getPageImages : Model -> List ImageLink -> List ImageLink
getPageImages model pictures =
    let
        startIndex : Int
        startIndex =
            (model.currentPage - 1) * model.itemsPerPage
    in
    List.take model.itemsPerPage (List.drop startIndex pictures)


renderButtons : Model -> List ImageLink -> Html Msg
renderButtons model pictures =
    div
        [ style "text-align" "center"
        , style "margin-top" "20px"
        ]
        [ button [ onClick PreviousPage, disabled (model.currentPage == 1) ] [ text "Previous" ]
        , button [ onClick NextPage, disabled (model.currentPage == numPages model pictures) ] [ text "Next" ]
        ]


numPages : Model -> List ImageLink -> Int
numPages model pictures =
    ceiling (toFloat (List.length pictures) / toFloat model.itemsPerPage)



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
        , expect = Http.expectJson GotPictures messageDecoder
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


imageLinksDecoder : Breed -> Maybe SubBreed -> Decoder (List String)
imageLinksDecoder breed maybeSubBreed =
    case maybeSubBreed of
        Nothing ->
            field (breedToString breed) (list Decode.string)

        Just subBreed ->
            field (subBreedToString subBreed) (list Decode.string)
