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
        , scale
        , words
        )



-- MAIN


type alias Memory =
    { height : Number
    , velocity : Number
    , enemies : List Enemy
    , colliding : Bool
    , score : Int
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
        , enemies =
            [ { right = 0, height = 0 }
            , { right = -300, height = 0 }
            , { right = -600, height = 0 }
            , { right = -800, height = 0 }
            ]
        , colliding = False
        , score = 0
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


getMessages : Screen -> Memory -> List Shape
getMessages screen memory =
    let
        score =
            (words (rgb 20 20 20) <|
                "Score: "
                    ++ String.fromInt (memory.score // 10)
            )
                |> move 0 (screen.top - 50)

        gameOver =
            scale 5 <|
                words (rgb 20 20 20) "GAME OVER"
    in
    if memory.colliding then
        [ score, gameOver ]

    else
        [ score ]


view : Computer -> Memory -> List Shape
view computer memory =
    [ rectangle (rgb 64 64 64) computer.screen.width 2
        |> moveY (getGroundY computer.screen)
    ]
        ++ List.map
            (\enemy ->
                rectangle (rgb 64 64 64) getEnemySize.x getEnemySize.y
                    |> move (getEnemyX computer.screen enemy) (getEnemyY computer.screen enemy)
            )
            memory.enemies
        ++ [ circle (rgb 20 20 20) getPlayerSize
                |> move (getPlayerX computer.screen) (getPlayerY computer.screen memory.height)
           ]
        ++ getMessages computer.screen memory



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

        enemies =
            List.map (updateEnemy computer.screen) memory.enemies
    in
    if memory.colliding then
        memory

    else
        { memory
            | height = height
            , velocity = velocity
            , enemies = enemies
            , colliding = isColliding computer.screen height enemies
            , score = memory.score + 1
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


isColliding : Screen -> Number -> List Enemy -> Bool
isColliding screen height enemies =
    let
        playerX =
            getPlayerX screen

        playerY =
            getPlayerY screen height
    in
    not <|
        List.isEmpty <|
            List.filter (\enemy -> isCollidingWithEnemy screen playerX playerY enemy) enemies


isCollidingWithEnemy : Screen -> Number -> Number -> Enemy -> Bool
isCollidingWithEnemy screen playerX playerY enemy =
    abs (playerX - getEnemyX screen enemy)
        < getEnemySize.x
        / 2
        && abs (playerY - getEnemyY screen enemy)
        < getEnemySize.y
        / 2
