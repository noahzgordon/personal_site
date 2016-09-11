module Main exposing (main)

import Html exposing (Html)
import Html.App exposing (program)
import Task
import Platform.Cmd exposing ((!))
import String
import Svg
import Svg.Attributes
import Window
import Ports


type alias Model =
    { viewportDimensions : Maybe ( Int, Int )
    , user : User
    }


type alias User =
    { coordinates : { x : Int, y : Int }
    }


type Message
    = WindowResize ( Int, Int )
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
    { viewportDimensions = Nothing
    , user = { coordinates = { x = 500, y = 500 } }
    }


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
            Svg.svg [ (viewBox dimensions model.user) ]
                [ user model.user
                ]


user : User -> Svg.Svg Message
user userElement =
    Svg.circle [ Svg.Attributes.cx (toString userElement.coordinates.x), Svg.Attributes.cy (toString userElement.coordinates.y), Svg.Attributes.r "10" ] []


viewBox : ( Int, Int ) -> User -> Svg.Attribute Message
viewBox dimensions user =
    let
        ( width, height ) =
            dimensions

        xPosition =
            user.coordinates.x - (width // 2)

        yPosition =
            user.coordinates.y - (height // 2)

        attributeString =
            [ xPosition, yPosition, width, height ]
              |> List.map toString
              |> String.join (" ")

    in
        Svg.Attributes.viewBox attributeString



-- subscriptions and tasks


subscriptions : Model -> Sub Message
subscriptions model =
    Window.resizes (\size -> WindowResize ( size.width, size.height ))


getWindowSize : Cmd Message
getWindowSize =
    Task.perform (\_ -> WindowSizeNotFound) (\size -> WindowResize ( size.width, size.height )) Window.size
