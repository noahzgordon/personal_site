module Keys exposing (..)


type Key
    = W
    | A
    | S
    | D
    | Invalid


fromCode : String -> Key
fromCode keyCode =
    case keyCode of
        "KeyW" ->
            W

        "KeyA" ->
            A

        "KeyS" ->
            S

        "KeyD" ->
            D

        _ ->
            Invalid


isValid : Key -> Bool
isValid =
    (/=) Invalid


delta : Key -> ( Float, Float )
delta key =
    case key of
        -- left
        A ->
            ( -20.0, 0.0 )

        -- up
        W ->
            ( 0.0, -20.0 )

        -- right
        D ->
            ( 20.0, 0.0 )

        -- down
        S ->
            ( 0.0, 20.0 )

        Invalid ->
            ( 0.0, 0.0 )
