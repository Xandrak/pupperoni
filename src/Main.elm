module Main exposing (Model, Msg, main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, div, h1, p, section, text)
import Html.Attributes exposing (href)
import Page.Breed as Breed
import Page.Breeds as Breeds
import Page.NotFound as NotFound
import Route exposing (Route(..))
import Url



-- MAIN


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
    , route : Route
    , page : Page
    }


type Page
    = Home
    | AllBreeds Breeds.Model
    | Breed Breed.Model
    | NotFound



-- add flags/check for session storage here


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Route.fromUrl url
    in
    ( Model key route Home, Cmd.none )


loadCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadCurrentPage ( model, cmd ) =
    let
        ( page, newCmd ) =
            case model.route of
                Route.AllBreeds ->
                    let
                        ( pageModel, pageCmd ) =
                            Breeds.init
                    in
                    ( AllBreeds pageModel, Cmd.map AllBreedsMsg pageCmd )

                Route.Home ->
                    ( Home, Cmd.none )

                Route.Breed breed ->
                    let
                        ( pageModel, pageCmd ) =
                            Breed.init (Breeds.stringToBreed breed) Nothing
                    in
                    ( Breed pageModel, Cmd.map BreedMsg pageCmd )

                Route.SubBreed breed subBreed ->
                    let
                        ( pageModel, pageCmd ) =
                            Breed.init (Breeds.stringToBreed breed) (Just <| Breeds.stringToSubBreed subBreed)
                    in
                    ( Breed pageModel, Cmd.map BreedMsg pageCmd )

                Route.NotFound ->
                    ( NotFound, Cmd.none )
    in
    ( { model | page = page }, Cmd.batch [ cmd, newCmd ] )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AllBreedsMsg Breeds.Msg
    | BreedMsg Breed.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.fromUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> loadCurrentPage

        ( AllBreedsMsg breedsMsg, AllBreeds breedsModel ) ->
            let
                ( newPageModel, newCmd ) =
                    Breeds.update breedsMsg breedsModel
            in
            ( { model | page = AllBreeds newPageModel }
            , Cmd.map AllBreedsMsg newCmd
            )

        ( AllBreedsMsg _, _ ) ->
            ( model, Cmd.none )

        ( BreedMsg breedMsg, Breed breedModel ) ->
            let
                ( newPageModel, newCmd ) =
                    Breed.update breedMsg breedModel
            in
            ( { model | page = Breed newPageModel }
            , Cmd.map BreedMsg newCmd
            )

        ( BreedMsg _, _ ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Puppy Power!"
    , body = [ currentView model ]
    }


currentView : Model -> Html Msg
currentView model =
    let
        page =
            case model.page of
                AllBreeds breedsModel ->
                    Breeds.view breedsModel
                        |> Html.map AllBreedsMsg

                Home ->
                    homeView

                Breed breedModel ->
                    Breed.view breedModel
                        |> Html.map BreedMsg

                NotFound ->
                    NotFound.view
    in
    section []
        [ nav model
        , page
        ]


nav : Model -> Html Msg
nav _ =
    div [] []


homeView : Html Msg
homeView =
    div []
        [ h1 [] [ text "Elm Example App - Dog Breeds" ]
        , p [] [ text "Click below to view a list of dog breeds with links to pictures!" ]
        , a [ Route.href Route.AllBreeds ] [ text "Click for puppy pics!" ]
        ]
