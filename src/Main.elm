module Main exposing (main)

import Html exposing (Html)
import Html.App exposing (program)
import Task
import Platform.Cmd exposing ((!))
import Platform.Sub
import String
import Svg
import Svg.Attributes
import Animation
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
    , style : Animation.State
    }


type Message
    = WindowResize ( Int, Int )
    | WindowSizeNotFound
    | KeyDown String
    | AnimateUser Animation.Msg


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
    , user = { kind = User, x = 500, y = 500, style = initialPosition ( 500.0, 500.0 ) }
    , elements =
        [ { kind = Wizard, x = 500, y = 200, style = initialPosition ( 500.0, 200.0 ) }
        ]
    }


initialPosition : ( Float, Float ) -> Animation.State
initialPosition coords =
    Animation.style [ Animation.cx (fst coords), Animation.cy (snd coords) ]


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        WindowResize dimensions ->
            { model | viewportDimensions = Just dimensions } ! []

        KeyDown code ->
            handleKeyDown code model ! []

        AnimateUser animMsg ->
            let
                user =
                    model.user

                animatedUser =
                    { user | style = Animation.update animMsg user.style }
            in
                { model | user = animatedUser } ! []

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
            let
                animation =
                    Animation.interrupt
                        [ Animation.to [ Animation.cx (toFloat newX), Animation.cy (toFloat newY) ]
                        ]
                        user.style

                animatedUser =
                    { user | style = animation, x = newX, y = newY }
            in
                { model | user = animatedUser }
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
    let
        elementColor =
            case element.kind of
                User ->
                    "red"

                Wizard ->
                    "blue"

        elementStyle =
            Animation.render element.style
                ++ [ Svg.Attributes.r "10"
                   , Svg.Attributes.fill elementColor
                   ]
    in
        Svg.circle elementStyle []


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
        , Animation.subscription AnimateUser [ model.user.style ]
        ]


getWindowSize : Cmd Message
getWindowSize =
    Task.perform (\_ -> WindowSizeNotFound) (\size -> WindowResize ( size.width, size.height )) Window.size
