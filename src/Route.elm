module Route exposing (Route(..), fromUrl, href, routeParser)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, map, oneOf, s)


type Route
    = Home
    | AllBreeds
    | Breed String
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home Parser.top
        , map AllBreeds (s "breeds")
        , map Breed (s "breed" </> Parser.string)
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

        NotFound ->
            "/404"


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)
