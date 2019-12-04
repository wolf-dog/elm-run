module Main exposing (main)

import Playground
    exposing
        ( Computer
        , Number
        , Shape
        , circle
        , game
        , move
        , moveY
        , rectangle
        , rgb
        )



-- MAIN


type alias Memory =
    { height : Number
    , velocity : Number
    }


main =
    game
        view
        update
        { height = 0
        , velocity = 0
        }



-- VIEW


view : Computer -> Memory -> List Shape
view computer memory =
    let
        width =
            computer.screen.width

        groundPosition =
            computer.screen.bottom + 100

        characterSize =
            20
    in
    [ rectangle (rgb 64 64 64) width 2
        |> moveY groundPosition
    , circle (rgb 20 20 20) characterSize
        |> move (0 - width / 2 + 100) (groundPosition + characterSize + memory.height)
    ]



-- UPDATE


update : Computer -> Memory -> Memory
update computer memory =
    let
        velocity =
            if memory.height > 0 then
                memory.velocity - 1.3

            else if computer.keyboard.up then
                20

            else
                0

        height =
            max 0 (memory.height + velocity)
    in
    { memory | height = height, velocity = velocity }
