module Main exposing (main)

import Html exposing (Html)
import Html.App exposing (program)
import Task
import Platform.Cmd exposing ((!))
import Svg
import Svg.Attributes
import Window
import Ports
import Data exposing (Dimensions)


type alias Model =
    { viewportDimensions : Maybe Dimensions
    }


type Message
    = WindowResize Dimensions
    | WindowSizeNotFound


main =
    program
        { init = initialModel ! [ getWindowSize ]
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initialModel : Model
initialModel =
    { viewportDimensions = Nothing }


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        WindowResize dimensions ->
            { model | viewportDimensions = Just dimensions } ! []

        _ ->
            model ! []


view : Model -> Html Message
view model =
    case model.viewportDimensions of
        Nothing ->
            Html.text "Loading..."

        Just dimensions ->
            Svg.svg [ (viewBox dimensions) ] []


viewBox : Dimensions -> Svg.Attribute Message
viewBox dimensions =
    let
        ( width, height ) =
            dimensions
    in
        Svg.Attributes.viewBox ("0 0 " ++ toString width ++ " " ++ toString height)



-- subscriptions and tasks


subscriptions : Model -> Sub Message
subscriptions model =
    Window.resizes (\size -> WindowResize ( size.width, size.height ))


getWindowSize : Cmd Message
getWindowSize =
    Task.perform (\_ -> WindowSizeNotFound) (\size -> WindowResize ( size.width, size.height )) Window.size
