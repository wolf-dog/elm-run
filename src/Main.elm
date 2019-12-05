module Main exposing (main)

import List.Extra exposing (findIndex)
import Playground
    exposing
        ( Computer
        , Keyboard
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
        , spin
        , words
        )



-- MAIN


type alias Enemies =
    List Enemy


type alias Memory =
    { height : Number
    , velocity : Number
    , enemies : Enemies
    , colliding : Bool
    , playing : Bool
    , score : Int
    , lastEmployedAt : Int
    }


type EnemyState
    = Working
    | Returning
    | Idle


type alias Enemy =
    { right : Number
    , height : Number
    , state : EnemyState
    }


main =
    game
        view
        update
        { height = 0
        , velocity = 0
        , enemies =
            { right = 0, height = 0, state = Working }
                :: List.repeat 10 { right = 0, height = 0, state = Idle }
        , playing = False
        , colliding = False
        , score = 0
        , lastEmployedAt = 0
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

        start =
            scale 3 <|
                words (rgb 20 20 20) "Press Enter Key to Start"

        gameOver =
            scale 5 <|
                words (rgb 20 20 20) "GAME OVER"
    in
    if not memory.playing then
        [ score, start ]

    else if memory.colliding then
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


type alias UpdatedEnemies =
    { enemies : Enemies
    , lastEmployedAt : Int
    }


getScrollSpeed : Number
getScrollSpeed =
    5


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
            updateEnemies computer.screen memory computer.time

        playing =
            if memory.playing then
                memory.playing

            else
                startPlaying computer.keyboard
    in
    if not playing || memory.colliding then
        memory

    else
        { memory
            | height = height
            , velocity = velocity
            , enemies = enemies.enemies
            , colliding = isColliding computer.screen height enemies.enemies
            , playing = playing
            , score = memory.score + 1
            , lastEmployedAt = enemies.lastEmployedAt
        }


updateEnemies : Screen -> Memory -> Playground.Time -> UpdatedEnemies
updateEnemies screen memory time =
    List.map (updateEnemy screen) memory.enemies |> employEnemy memory time


updateEnemy : Screen -> Enemy -> Enemy
updateEnemy screen enemy =
    let
        state =
            getEnemyState screen enemy

        right =
            case state of
                Working ->
                    enemy.right + getScrollSpeed

                Returning ->
                    0

                Idle ->
                    0
    in
    { enemy | right = right, state = state }


getEnemyState : Screen -> Enemy -> EnemyState
getEnemyState screen enemy =
    case enemy.state of
        Working ->
            if getEnemyX screen enemy <= screen.left then
                Returning

            else
                Working

        Returning ->
            Idle

        Idle ->
            Idle


employEnemy : Memory -> Playground.Time -> Enemies -> UpdatedEnemies
employEnemy memory time enemies =
    let
        firstIdleEnemyIndex =
            findIndex (\enemy -> enemy.state == Idle) enemies
    in
    if
        memory.score
            > memory.lastEmployedAt
            + 31
            && memory.score
            + (round <| abs <| cos (spin 0.1 time) * 150)
            > memory.lastEmployedAt
            + 181
    then
        case firstIdleEnemyIndex of
            Just firstIndex ->
                { enemies =
                    List.indexedMap
                        (\index enemy ->
                            if index == firstIndex then
                                { enemy | state = Working }

                            else
                                enemy
                        )
                        enemies
                , lastEmployedAt = memory.score
                }

            Nothing ->
                { enemies = enemies
                , lastEmployedAt = memory.lastEmployedAt
                }

    else
        { enemies = enemies
        , lastEmployedAt = memory.lastEmployedAt
        }


isColliding : Screen -> Number -> Enemies -> Bool
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


startPlaying : Keyboard -> Bool
startPlaying keyboard =
    if keyboard.enter then
        True

    else
        False
