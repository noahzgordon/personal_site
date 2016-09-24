module Main exposing (main)

import Html exposing (Html)
import Html.App exposing (program)
import Task
import Platform.Cmd exposing ((!))
import Platform.Sub
import String
import List.Extra as List
import Svg
import Svg.Attributes
import Animation
import Window
import Ports


type alias Model =
    { viewBox : Maybe ViewBox
    , user : Element
    , elements : List Element
    }


type alias ViewBox =
    { dimensions : ( Int, Int )
    , style : Animation.State
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
    | AnimateViewBox Animation.Msg


main =
    program
        { init = initialModel ! [ getWindowSize ]
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initialModel : Model
initialModel =
    { viewBox = Nothing
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
            let
                viewBox =
                    { dimensions = dimensions, style = Animation.style (viewBoxProperties dimensions (model.user.x, model.user.y)) }
            in
                { model | viewBox = Just viewBox } ! []

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

        AnimateViewBox animMsg ->
            case model.viewBox of
                Nothing ->
                    model ! []

                Just viewBox ->
                    let
                        animatedViewBox =
                            { viewBox | style = Animation.update animMsg viewBox.style }
                    in
                        { model | viewBox = Just animatedViewBox } ! []

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
                applyUserAnimation =
                    Animation.interrupt [ Animation.to [ Animation.cx (toFloat newX), Animation.cy (toFloat newY) ] ]

                animatedUser =
                    { user | style = applyUserAnimation user.style, x = newX, y = newY }
            in
                { model | user = animatedUser, viewBox = animateViewBox model ( newX, newY ) }
        else
            model


animateViewBox : Model -> ( Int, Int ) -> Maybe ViewBox
animateViewBox model coords =
    case model.viewBox of
        Nothing ->
            Nothing

        Just viewBox ->
            let
                newProperties =
                    viewBoxProperties viewBox.dimensions coords

                animation =
                    Animation.interrupt [ Animation.to newProperties ] viewBox.style
            in
                Just { viewBox | style = animation }


isOccupied : List Element -> ( Int, Int ) -> Bool
isOccupied elements coords =
    List.any (\elem -> ( elem.x, elem.y ) == coords) elements



-- view


view : Model -> Html Message
view model =
    case model.viewBox of
        Nothing ->
            Html.text "Loading..."

        Just viewBox ->
            Svg.svg (Animation.render viewBox.style)
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



-- Animation.Property is not yet exposed
-- viewBoxProperties : ( Int, Int ) -> Element -> List Animation.Property


viewBoxProperties dimensions userCoords =
    let
        ( width, height ) =
            dimensions

        xPosition =
            fst userCoords - (width // 2) |> toFloat

        yPosition =
            snd userCoords - (height // 2) |> toFloat
    in
        [ Animation.viewBox xPosition yPosition (toFloat width) (toFloat height) ]


attributeString : List x -> String
attributeString =
    List.map toString >> String.join (" ")



-- subscriptions and tasks


subscriptions : Model -> Sub Message
subscriptions model =
    let
        viewBoxStyleArr =
            case model.viewBox of
                Nothing ->
                    []

                Just viewBox ->
                    [ viewBox.style ]
    in
        Platform.Sub.batch
            [ Window.resizes (\size -> WindowResize ( size.width, size.height ))
            , Ports.keyboard KeyDown
            , Animation.subscription AnimateUser [ model.user.style ]
            , Animation.subscription AnimateViewBox viewBoxStyleArr
            ]


getWindowSize : Cmd Message
getWindowSize =
    Task.perform (\_ -> WindowSizeNotFound) (\size -> WindowResize ( size.width, size.height )) Window.size
