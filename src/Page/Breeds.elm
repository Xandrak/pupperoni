module Page.Breeds exposing (Breed, Model, Msg, SubBreed, breedToString, init, stringToBreed, stringToSubBreed, subBreedToString, update, view)

import Helpers.Message as Message
import Html exposing (Html, a, div, h1, li, p, text, ul)
import Http
import Json.Decode as Decode exposing (Decoder, field, keyValuePairs, list)
import Route as Route



-- MODEL


type alias Model =
    { dogBreeds : BreedRequest
    }


init : ( Model, Cmd Msg )
init =
    ( Model Loading
    , getAllBreeds GotBreeds
    )



-- TYPES


type BreedRequest
    = Failure Http.Error
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
                        sortedBreeds =
                            List.sort breeds

                        convertedBreeds =
                            List.map convertFromStringToBreed sortedBreeds
                    in
                    -- persist in session storage here as well
                    ( { model | dogBreeds = Success convertedBreeds }, Cmd.none )

                Err err ->
                    ( { model | dogBreeds = Failure err }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Dog Breeds" ]
        , p [] [ text "Click a breed (or sub-breed) to view some puppy pics!" ]
        , case model.dogBreeds of
            Loading ->
                div [] [ text Message.loading ]

            Failure _ ->
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
        convertedBreed =
            stringToBreed breed

        convertedSubBreeds =
            List.map stringToSubBreed subBreeds
    in
    ( convertedBreed, convertedSubBreeds )


unorderedBreedLinkList : ( Breed, List SubBreed ) -> Html Msg
unorderedBreedLinkList ( breedName, subBreeds ) =
    let
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
        breedNameStr =
            breedToString breedName

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
