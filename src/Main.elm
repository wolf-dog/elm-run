module Main exposing (main)

import Playground
    exposing
        ( circle
        , game
        , move
        , moveY
        , rectangle
        , rgb
        )



-- MAIN


main =
    game
        view
        update
        0



-- VIEW


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
        |> move (0 - width / 2 + 100) (groundPosition + characterSize)
    ]



-- UPDATE


update computer memory =
    0
