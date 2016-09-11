module Main exposing (main)

import Html exposing (Html)
import Html.App exposing (program)
import Task
import Platform.Cmd exposing ((!))
import Platform.Sub
import String
import Svg
import Svg.Attributes
import Window
import Keyboard
import Ports


type alias Model =
    { viewportDimensions : Maybe ( Int, Int )
    , user : User
    }


type alias User =
    { x : Int
    , y : Int
    }


type Message
    = WindowResize ( Int, Int )
    | WindowSizeNotFound
    | KeyPress Keyboard.KeyCode


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
    , user = { x = 500, y = 500 }
    }


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        WindowResize dimensions ->
            { model | viewportDimensions = Just dimensions } ! []

        KeyPress keyCode ->
            handleKeyPress keyCode model ! []

        _ ->
            model ! []


handleKeyPress : Keyboard.KeyCode -> Model -> Model
handleKeyPress keyCode model =
    let
        delta =
            case keyCode of
                97 -> ( -10, 0 )
                119 -> ( 0, 10 )
                100 -> ( 10, 0 )
                115 -> ( 0, -10 )
                _ -> ( 0, 0 )
    in
        { model | user = adjustCoordinates model.user delta }


adjustCoordinates : User -> ( Int, Int ) -> User
adjustCoordinates player delta =
  let
      ( xDelta, yDelta ) = delta
  in
      { player | x = player.x + xDelta, y = player.y + yDelta }


-- view


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
    Svg.circle [ Svg.Attributes.cx (toString userElement.x), Svg.Attributes.cy (toString userElement.y), Svg.Attributes.r "10" ] []


viewBox : ( Int, Int ) -> User -> Svg.Attribute Message
viewBox dimensions user =
    let
        ( width, height ) =
            dimensions

        xPosition =
            user.x - (width // 2)

        yPosition =
            user.y - (height // 2)
    in
        Svg.Attributes.viewBox (attributeString [ xPosition, yPosition, width, height ])


attributeString : List x -> String
attributeString list =
    list
        |> List.map toString
        |> String.join (" ")



-- subscriptions and tasks


subscriptions : Model -> Sub Message
subscriptions model =
    Platform.Sub.batch
        [ Window.resizes (\size -> WindowResize ( size.width, size.height ))
        , Keyboard.presses KeyPress
        ]


getWindowSize : Cmd Message
getWindowSize =
    Task.perform (\_ -> WindowSizeNotFound) (\size -> WindowResize ( size.width, size.height )) Window.size
