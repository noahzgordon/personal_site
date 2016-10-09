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
import Keys
import Ports


roomSize =
    ( 1000.0, 1000.0 )


userStart =
    ( 500.0, 500.0 )


type alias Model =
    { viewBox : Maybe ViewBox
    , user : Element
    , elements : List Element
    }


type alias ViewBox =
    { dimensions : ( Float, Float )
    , style : Animation.State
    }


type ElementKind
    = User
    | Wizard


type alias Element =
    { kind : ElementKind
    , x : Float
    , y : Float
    , style : Animation.State
    }


type Message
    = WindowResize { width : Int, height : Int }
    | WindowSizeNotFound
    | KeyDown Keys.Key
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
    , user = { kind = User, x = 500.0, y = 500.0, style = initialPosition userStart }
    , elements =
        [ { kind = Wizard, x = 500.0, y = 200.0, style = initialPosition ( 500.0, 200.0 ) }
        ]
    }


initialPosition : ( Float, Float ) -> Animation.State
initialPosition coords =
    Animation.style [ Animation.translate (Animation.px (fst coords)) (Animation.px (snd coords)) ]


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        WindowResize size ->
            let
                dimensions =
                    ( toFloat size.width, toFloat size.height )

                viewBoxStyle =
                    viewBoxProperties dimensions ( model.user.x, model.user.y )
                        |> Animation.style

                viewBox =
                    { dimensions = dimensions, style = viewBoxStyle }
            in
                { model | viewBox = Just viewBox } ! []

        KeyDown key ->
            if Keys.isValid key then
                handleKeyDown key model ! []
            else
                model ! []

        AnimateUser animationMessage ->
            { model | user = animate animationMessage model.user } ! []

        AnimateViewBox animationMessage ->
            { model | viewBox = Maybe.map (animate animationMessage) model.viewBox } ! []

        _ ->
            model ! []


animate : Animation.Msg -> { a | style : Animation.State } -> { a | style : Animation.State }
animate message record =
    { record | style = Animation.update message record.style }


handleKeyDown : Keys.Key -> Model -> Model
handleKeyDown key model =
    let
        ( deltaX, deltaY ) =
            Keys.delta key

        angle =
            Keys.angle key

        ( turnX, turnY ) =
            Keys.turnTransform key

        user =
            model.user

        newX =
            user.x + deltaX

        newY =
            user.y + deltaY
    in
        if (isEmpty model ( newX, newY )) && (isWithinRoom ( newX, newY )) then
            let
                applyUserAnimation =
                    Animation.interrupt
                        [ Animation.set
                          [ Animation.translate (Animation.px (user.x + turnX)) (Animation.px (user.y + turnY))
                          , Animation.rotate (Animation.deg angle)
                          ]
                        , Animation.to [ Animation.translate (Animation.px (newX + turnX)) (Animation.px (newY + turnY)) ]
                        ]

                animatedUser =
                    { user | style = applyUserAnimation user.style, x = newX, y = newY }
            in
                { model | user = animatedUser, viewBox = animateViewBox model ( newX, newY ) }
        else
            model


animateViewBox : Model -> ( Float, Float ) -> Maybe ViewBox
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


isEmpty : Model -> ( Float, Float ) -> Bool
isEmpty { elements } coords =
    List.any (\elem -> ( elem.x, elem.y ) == coords) elements
        |> not


isWithinRoom : ( Float, Float ) -> Bool
isWithinRoom ( x, y ) =
    x > 0 && y > 0 && x < (fst roomSize) && y < (fst roomSize)



-- view


view : Model -> Html Message
view model =
    case model.viewBox of
        Nothing ->
            Html.text "Loading..."

        Just viewBox ->
            Svg.svg (Animation.render viewBox.style)
                ([ Svg.rect
                    [ Svg.Attributes.width (fst roomSize |> toString)
                    , Svg.Attributes.height (snd roomSize |> toString)
                    , Svg.Attributes.x "0"
                    , Svg.Attributes.y "0"
                    ]
                    []
                 ]
                    ++ (List.map renderElement ([ model.user ] ++ model.elements))
                )


renderElement : Element -> Svg.Svg Message
renderElement element =
    case element.kind of
        User ->
            let
                userStyle =
                    Animation.render element.style
                        ++ [ Svg.Attributes.fill "red"
                           , Svg.Attributes.points "0,20 10,0 20,20"
                           ]
            in
                Svg.polygon userStyle []

        Wizard ->
            let
                wizardStyle =
                    Animation.render element.style
                        ++ [ Svg.Attributes.width "20"
                           , Svg.Attributes.height "20"
                           , Svg.Attributes.fill "blue"
                           ]
            in
                Svg.rect wizardStyle []



-- Animation.Property is not yet exposed
-- viewBoxProperties : ( Int, Int ) -> Element -> List Animation.Property


viewBoxProperties dimensions userCoords =
    let
        ( width, height ) =
            dimensions

        xPosition =
            fst userCoords - (width / 2)

        yPosition =
            snd userCoords - (height / 2)
    in
        [ Animation.viewBox xPosition yPosition width height ]


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
            [ Window.resizes WindowResize
            , Ports.keyboard (KeyDown << Keys.fromCode)
            , Animation.subscription AnimateUser [ model.user.style ]
            , Animation.subscription AnimateViewBox viewBoxStyleArr
            ]


getWindowSize : Cmd Message
getWindowSize =
    Task.perform (always WindowSizeNotFound) WindowResize Window.size
