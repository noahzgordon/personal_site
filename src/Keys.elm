module Keys exposing (..)


type KeyAction
    = Movement MovementAction
    | Invalid


type MovementAction
    = Up
    | Down
    | Left
    | Right


actionFromCode : String -> KeyAction
actionFromCode keyCode =
    case keyCode of
        "KeyW" ->
            Movement Up

        "KeyA" ->
            Movement Left

        "KeyS" ->
            Movement Down

        "KeyD" ->
            Movement Right

        _ ->
            Invalid


type alias MovementProperties =
    { angle : Float
    , vector : ( Float, Float )
    , angleAdjustment : ( Float, Float )
    }


movementProperties : MovementAction -> MovementProperties
movementProperties key =
    case key of
        Up ->
            { angle = 0
            , vector = ( 0, -20 )
            , angleAdjustment = ( 0, 0 )
            }

        Down ->
            { angle = 180
            , vector = ( 0, 20 )
            , angleAdjustment = ( 20, 20 )
            }

        Left ->
            { angle = 270
            , vector = ( -20, 0 )
            , angleAdjustment = ( 0, 20 )
            }

        Right ->
            { angle = 90
            , vector = ( 20, 0 )
            , angleAdjustment = ( 20, 0 )
            }
