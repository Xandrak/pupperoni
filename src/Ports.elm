port module Ports exposing (setStorage)

import Json.Encode as Encode


port setStorage : Encode.Value -> Cmd msg
