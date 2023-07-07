module Page.Breeds exposing
    ( Breed
    , BreedRequest
    , Model
    , Msg
    , SubBreed
    , breedToString
    , init
    , stringToBreed
    , stringToSubBreed
    , subBreedToString
    , update
    , view
    )

import Helpers.Message as Message
import Html exposing (Html, a, div, h1, li, p, text, ul)
import Html.Attributes exposing (style)
import Http
import Json.Decode as Decode exposing (Decoder, field, keyValuePairs, list)
import Json.Encode as Encode
import Ports
import Route



-- MODEL


type alias Model =
    { dogBreeds : BreedRequest
    }


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue messageDecoder flags of
        Ok breeds ->
            let
                convertedBreeds : List ( Breed, List SubBreed )
                convertedBreeds =
                    List.map convertFromStringToBreed breeds
            in
            ( Model (Success convertedBreeds), Cmd.none )

        Err _ ->
            ( Model Loading
            , getAllBreeds GotBreeds
            )



-- TYPES


type BreedRequest
    = Failure
    | Loading
    | Success (List ( Breed, List SubBreed ))


type Breed
    = Breed String


type SubBreed
    = SubBreed String


type Msg
    = GotBreeds (Result Http.Error (List ( String, List String )))



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBreeds result ->
            case result of
                Ok breeds ->
                    let
                        sortedBreeds : List ( String, List String )
                        sortedBreeds =
                            List.sort breeds

                        convertedBreeds : List ( Breed, List SubBreed )
                        convertedBreeds =
                            List.map convertFromStringToBreed sortedBreeds

                        encodedBreeds : Encode.Value
                        encodedBreeds =
                            allBreedsEncoder convertedBreeds
                    in
                    -- persist in session storage here as well
                    ( { model | dogBreeds = Success convertedBreeds }, Ports.setStorage encodedBreeds )

                Err _ ->
                    ( { model | dogBreeds = Failure }, Cmd.none )



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
        , p [] [ text "Click a breed (or sub-breed) to view some puppy pics!" ]
        , case model.dogBreeds of
            Loading ->
                div [] [ text Message.loading ]

            Failure ->
                div [] [ text Message.failureToFetch ]

            Success breeds ->
                div [] <| List.map unorderedBreedLinkList breeds
        ]



-- HELPERS


stringToBreed : String -> Breed
stringToBreed str =
    Breed str


breedToString : Breed -> String
breedToString (Breed str) =
    str


stringToSubBreed : String -> SubBreed
stringToSubBreed str =
    SubBreed str


subBreedToString : SubBreed -> String
subBreedToString (SubBreed str) =
    str


convertFromStringToBreed : ( String, List String ) -> ( Breed, List SubBreed )
convertFromStringToBreed ( breed, subBreeds ) =
    let
        convertedBreed : Breed
        convertedBreed =
            stringToBreed breed

        convertedSubBreeds : List SubBreed
        convertedSubBreeds =
            List.map stringToSubBreed subBreeds
    in
    ( convertedBreed, convertedSubBreeds )


unorderedBreedLinkList : ( Breed, List SubBreed ) -> Html Msg
unorderedBreedLinkList ( breedName, subBreeds ) =
    let
        breedNameStr : String
        breedNameStr =
            breedToString breedName
    in
    case subBreeds of
        [] ->
            li [] [ a [ Route.href (Route.Breed breedNameStr) ] [ text breedNameStr ] ]

        _ ->
            li []
                [ a [ Route.href (Route.Breed breedNameStr) ] [ text breedNameStr ]
                , ul [] <| List.map (makeSubBreedListLink breedName) subBreeds
                ]



-- make types for breed and subbreed to prevent type blindness here


makeSubBreedListLink : Breed -> SubBreed -> Html Msg
makeSubBreedListLink breedName subBreed =
    let
        breedNameStr : String
        breedNameStr =
            breedToString breedName

        subBreedNameStr : String
        subBreedNameStr =
            subBreedToString subBreed
    in
    li [] [ a [ Route.href (Route.SubBreed breedNameStr subBreedNameStr) ] [ text subBreedNameStr ] ]



-- HTTP


getAllBreeds : (Result Http.Error (List ( String, List String )) -> msg) -> Cmd msg
getAllBreeds expectedMsg =
    Http.get
        { url = "https://dog.ceo/api/breeds/list/all"
        , expect = Http.expectJson expectedMsg messageDecoder
        }


messageDecoder : Decoder (List ( String, List String ))
messageDecoder =
    field "message" (keyValuePairs (list Decode.string))


allBreedsEncoder : List ( Breed, List SubBreed ) -> Encode.Value
allBreedsEncoder breeds =
    Encode.object
        [ ( "message", Encode.list encodeBreeds breeds )
        ]


encodeBreeds : ( Breed, List SubBreed ) -> Encode.Value
encodeBreeds ( breed, subBreeds ) =
    Encode.object
        [ ( breedToString breed, Encode.list Encode.string <| List.map subBreedToString subBreeds )
        ]
