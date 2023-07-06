module Routes exposing (Route, routeParser)

import Url.Parser as Parser exposing ((</>), Parser, map, oneOf, s)
type Route
    = Breed String


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Breed (s "breed" </> Parser.string)
        ]
