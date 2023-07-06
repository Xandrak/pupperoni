module Page.Breeds exposing (..)

import Api.Breeds exposing (BreedRequest(..), getAllBreeds)
import Html exposing (Html, a, div, h1, li, p, text, ul)
import Html.Attributes exposing (href)
import Http


type alias Model =
    { dogBreeds : BreedRequest
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Model Loading
    , getAllBreeds GotBreeds
    )


type Msg
    = GotBreeds (Result Http.Error (List ( String, List String )))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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


view : Model -> List (Html Msg)
view model =
    [ div []
        [ h1 [] [ text "Dog Breeds" ]
        , p [] [ text "Select a breed (or sub-breed) to view some puppy pics!" ]
        ,   case model.dogBreeds of
                Loading ->
                    div [] [ text "Sorry for the brief paws--we're loading! ;)" ]

                Failure _ ->
                    div [] [ text "Dog-gonit! Something went wrong with that fetch. Let's try again." ]

                Success breeds ->
                    div [] <| List.map createUnorderedLinkList breeds
        ]
    ]


createUnorderedLinkList : ( String, List String ) -> Html Msg
createUnorderedLinkList ( breed, subBreeds ) =
    let
        path =
            "breed/" ++ breed
    in
    case subBreeds of
        [] ->
            li [] [ a [ href path ] [ text breed ] ]

        _ ->
            li []
                [ text breed
                , ul [] <| List.map (makeSubListLink path) subBreeds
                ]



-- make types for breed and subbreed to prevent type blindness here


makeSubListLink : String -> String -> Html Msg
makeSubListLink path subBreed =
    let
        subBreedPath =
            path ++ "/" ++ subBreed
    in
    li [] [ a [ href subBreedPath ] [ text subBreed ] ]
