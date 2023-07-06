module Page.NotFound exposing (view)

import Html exposing (Html, div, text)


view : Html msg
view =
    div [] [ text "Ruh-Roh, something went wrong!" ]
