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
import Ports


type alias Model =
    { viewportDimensions : Maybe ( Int, Int )
    , user : Element
    , elements : List Element
    }


type ElementKind
    = User
    | Wizard


type alias Element =
    { kind : ElementKind
    , x : Int
    , y : Int
    }


type Message
    = WindowResize ( Int, Int )
    | WindowSizeNotFound
    | KeyDown String


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
    , user = { kind = User, x = 500, y = 500 }
    , elements =
        [ { kind = Wizard, x = 500, y = 200 }
        ]
    }


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        WindowResize dimensions ->
            { model | viewportDimensions = Just dimensions } ! []

        KeyDown code ->
            handleKeyDown code model ! []

        _ ->
            model ! []


handleKeyDown : String -> Model -> Model
handleKeyDown code model =
    let
        delta =
            case code of
                -- left
                "KeyA" ->
                    ( -20, 0 )

                -- up
                "KeyW" ->
                    ( 0, -20 )

                -- right
                "KeyD" ->
                    ( 20, 0 )

                -- down
                "KeyS" ->
                    ( 0, 20 )

                _ ->
                    ( 0, 0 )

        user =
            model.user

        newX =
            user.x + fst delta

        newY =
            user.y + snd delta
    in
        if not (isOccupied model.elements ( newX, newY )) then
            { model | user = { user | x = newX, y = newY } }
        else
            model


isOccupied : List Element -> ( Int, Int ) -> Bool
isOccupied elements coords =
    List.any (\elem -> ( elem.x, elem.y ) == coords) elements



-- view


view : Model -> Html Message
view model =
    case model.viewportDimensions of
        Nothing ->
            Html.text "Loading..."

        Just dimensions ->
            Svg.svg [ (viewBox dimensions model.user) ]
                (List.map renderElement ([ model.user ] ++ model.elements))


renderElement : Element -> Svg.Svg Message
renderElement element =
    case element.kind of
        User ->
            Svg.circle [ Svg.Attributes.cx (toString element.x), Svg.Attributes.cy (toString element.y), Svg.Attributes.r "10" ] []

        Wizard ->
            Svg.circle [ Svg.Attributes.cx (toString element.x), Svg.Attributes.cy (toString element.y), Svg.Attributes.r "10", Svg.Attributes.fill "blue" ] []


viewBox : ( Int, Int ) -> Element -> Svg.Attribute Message
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
        , Ports.keyboard KeyDown
        ]


getWindowSize : Cmd Message
getWindowSize =
    Task.perform (\_ -> WindowSizeNotFound) (\size -> WindowResize ( size.width, size.height )) Window.size
