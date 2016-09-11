module Main exposing (main)

import Html exposing (Html)
import Html.App exposing (program)
import Platform.Cmd exposing ((!))


type alias Model = {}


type alias Message = {}


main = program
    { init = Model ! []
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    ( model, Cmd.none )


view : Model -> Html Message
view model =
    Html.text "Hello World"
