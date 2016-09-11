port module Ports exposing (..)

import Data exposing (Dimensions)


port dimensions : (Dimensions -> msg) -> Sub msg
