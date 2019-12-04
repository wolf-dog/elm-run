module Main exposing (main)

import Playground
    exposing
        ( Computer
        , Number
        , Screen
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
    , enemy : Enemy
    }


type alias Enemy =
    { right : Number
    , height : Number
    }


main =
    game
        view
        update
        { height = 0
        , velocity = 0
        , enemy = { right = 0, height = 0 }
        }



-- VIEW


getGroundY : Screen -> Number
getGroundY screen =
    screen.bottom + 100


getPlayerSize : Number
getPlayerSize =
    20


getPlayerX : Screen -> Number
getPlayerX screen =
    screen.left + 100


getPlayerY : Screen -> Number -> Number
getPlayerY screen height =
    getGroundY screen + getPlayerSize + height


getEnemySize : { x : Number, y : Number }
getEnemySize =
    { x = 40, y = 150 }


getEnemyX : Screen -> Enemy -> Number
getEnemyX screen enemy =
    screen.right + getEnemySize.x / 2 - enemy.right


getEnemyY : Screen -> Enemy -> Number
getEnemyY screen enemy =
    getGroundY screen + getEnemySize.y / 2 + enemy.height


view : Computer -> Memory -> List Shape
view computer memory =
    [ rectangle (rgb 64 64 64) computer.screen.width 2
        |> moveY (getGroundY computer.screen)
    , rectangle (rgb 64 64 64) getEnemySize.x getEnemySize.y
        |> move (getEnemyX computer.screen memory.enemy) (getEnemyY computer.screen memory.enemy)
    , circle (rgb 20 20 20) getPlayerSize
        |> move (getPlayerX computer.screen) (getPlayerY computer.screen memory.height)
    ]



-- UPDATE


type EnemyState
    = Working
    | Returning


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
    { memory
        | height = height
        , velocity = velocity
        , enemy = updateEnemy computer.screen memory.enemy
    }


updateEnemy : Screen -> Enemy -> Enemy
updateEnemy screen enemy =
    let
        state =
            getEnemyState screen enemy

        right =
            case state of
                Working ->
                    enemy.right + 5

                Returning ->
                    0
    in
    { enemy | right = right }


getEnemyState : Screen -> Enemy -> EnemyState
getEnemyState screen enemy =
    if getEnemyX screen enemy <= screen.left then
        Returning

    else
        Working
