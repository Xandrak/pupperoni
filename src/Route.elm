module Route exposing (Route(..), fromUrl, href)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, map, oneOf, s)


type Route
    = Home
    | AllBreeds
    | Breed String
    | SubBreed String String
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home Parser.top
        , map AllBreeds (s "breeds")
        , map Breed (s "breed" </> Parser.string)
        , map SubBreed (s "breed" </> Parser.string </> Parser.string)
        ]


fromUrl : Url -> Route
fromUrl url =
    case Parser.parse routeParser url of
        Just route ->
            route

        Nothing ->
            NotFound


routeToString : Route -> String
routeToString route =
    case route of
        Home ->
            "/"

        AllBreeds ->
            "/breeds"

        Breed breed ->
            "/breed/" ++ breed

        SubBreed breed subBreed ->
            "/breed/" ++ breed ++ "/" ++ subBreed

        NotFound ->
            "/404"


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)
