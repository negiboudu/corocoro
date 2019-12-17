module Main exposing (main)

import Playground exposing (..)


main =
    game view update init


type alias Memory =
    { g : Float
    , x : Float
    , y : Float
    , xmove : Float
    , ymove : Float
    , angle : Int
    , jumpcount : Int
    , jumppower : Float
    , scene : Scene
    , screenBorderWidth : Float
    , charactorWidth : Float
    , stage : Int
    , finalstage : Int
    , blocks : List ( Float, Float )
    , blockWidth : Float
    }


init : Memory
init =
    { g = 0.2
    , x = Tuple.first startPosition
    , y = Tuple.second startPosition
    , xmove = 1
    , ymove = 0
    , angle = 0
    , jumpcount = 0
    , jumppower = 5
    , scene = Title
    , screenBorderWidth = 2
    , charactorWidth = 20
    , stage = 1
    , finalstage = 7
    , blocks = createStage 1
    , blockWidth = 20
    }


startPosition : ( Float, Float )
startPosition =
    ( -200, -200 )


goalPosition : Float
goalPosition =
    200


createStage : Int -> List ( Float, Float )
createStage stage =
    case stage of
        1 ->
            []

        2 ->
            [ ( 0, -200 ) ]

        3 ->
            [ ( 0, -200 )
            , ( 0, -170 )
            , ( 0, -140 )
            ]

        4 ->
            [ ( 200, -200 )
            , ( 200, -170 )
            , ( 200, -140 )
            , ( 200, -110 )
            , ( 200, -80 )
            , ( 130, -170 )
            , ( 100, -170 )
            , ( 70, -170 )
            , ( 70, -140 )
            , ( 70, -110 )
            , ( 70, -80 )
            ]

        5 ->
            [ ( 200, -200 )
            , ( 200, -170 )
            , ( 200, -140 )
            , ( 200, -110 )
            , ( -110, -170 )
            , ( -90, -170 )
            , ( -30, -130 )
            , ( -10, -130 )
            , ( 50, -90 )
            , ( 70, -90 )
            ]

        6 ->
            [ ( 0, -170 )
            , ( 20, -170 )
            , ( 20, -150 )
            , ( -60, -120 )
            , ( -80, -120 )
            , ( -80, -100 )
            , ( 0, -70 )
            , ( 20, -70 )
            , ( 20, -50 )
            , ( -60, -20 )
            , ( -80, -20 )
            , ( -80, 0 )
            , ( 0, 30 )
            , ( 20, 30 )
            , ( 80, -200 )
            , ( 80, -170 )
            , ( 80, -140 )
            , ( 80, -110 )
            , ( 80, -80 )
            , ( 80, -50 )
            , ( 80, -20 )
            , ( 80, 10 )
            , ( 80, 40 )
            , ( 80, 70 )
            , ( 80, 100 )
            ]

        7 ->
            [ ( 180, -185 )
            , ( 180, -155 )
            , ( 180, -125 )
            , ( 180, -95 )
            , ( 180, -65 )
            , ( 180, -35 )
            , ( 180, -10 )
            , ( 130, -170 )
            , ( 70, -130 )
            , ( 100, -60 )
            , ( 20, -80 )
            , ( -40, 0 )
            ]

        _ ->
            []


gametitle : String
gametitle =
    "Corocoro"


stageclearMsg : String
stageclearMsg =
    "CLEAR!"


type Scene
    = Title
    | Running
    | Goal
    | Ending


type HitPoint
    = Top
    | Bottom
    | Left
    | Right


view : Computer -> Memory -> List Shape
view computer memory =
    case memory.scene of
        Title ->
            [ words grey gametitle
                |> scale 5
                |> move 5 -5
            , words green gametitle
                |> scale 5
            , words darkGreen
                "Click to start"
                |> scale 2
                |> move 0 -100
            ]

        Running ->
            runningView computer memory

        Goal ->
            runningView computer memory
                ++ [ words grey stageclearMsg
                        |> scale 5
                        |> move 0 30
                   , words lightYellow stageclearMsg
                        |> scale 5
                        |> move -5 35
                   , words darkYellow "Click to next stage"
                        |> move 0 -50
                   ]

        Ending ->
            [ words
                (rgb
                    (zigzag 0 240 3 computer.time)
                    (zigzag 0 240 2 computer.time)
                    (zigzag 0 240 1 computer.time)
                )
                "Congratulations!"
                |> scale 3
                |> move 0 30
            , words
                (rgb
                    (zigzag 0 200 1.5 computer.time)
                    (zigzag 0 200 1 computer.time)
                    (zigzag 0 200 0.5 computer.time)
                )
                "The end"
                |> scale 2
                |> move 0 -30
            ]


runningView : Computer -> Memory -> List Shape
runningView computer memory =
    [ words black ("STAGE " ++ String.fromInt memory.stage)
        |> move -150 200
    , square green memory.charactorWidth
        |> move memory.x memory.y
        |> rotate (toFloat memory.angle)
    , rectangle lightOrange memory.screenBorderWidth computer.screen.height
        |> move (goalPosition + memory.charactorWidth / 2) 0
    , rectangle darkGrey computer.screen.width memory.screenBorderWidth
        |> move 0 (Tuple.second startPosition - memory.charactorWidth / 2)
    , rectangle darkGrey memory.screenBorderWidth computer.screen.height
        |> move (Tuple.first startPosition - memory.charactorWidth / 2) 0
    ]
        ++ List.map
            (\( x, y ) ->
                square darkGrey memory.blockWidth
                    |> move x y
            )
            memory.blocks


update : Computer -> Memory -> Memory
update computer memory =
    case memory.scene of
        Title ->
            if computer.mouse.click then
                { memory
                    | scene = Running
                }

            else
                memory

        Running ->
            memory
                |> jumpCharactor computer
                |> gravity
                |> moveCharactor
                |> checkGround
                |> checkHitStartWall
                |> (\mem -> List.foldl isHit mem mem.blocks)
                |> checkGoal

        Goal ->
            if computer.mouse.click then
                if memory.stage == memory.finalstage then
                    { memory
                        | scene = Ending
                    }

                else
                    { memory
                        | scene = Running
                        , x = Tuple.first startPosition
                        , y = Tuple.second startPosition
                        , xmove = 1
                        , ymove = 0
                        , jumpcount = 0
                        , stage = memory.stage + 1
                        , blocks = createStage (memory.stage + 1)
                    }

            else
                memory

        Ending ->
            memory


jumpCharactor : Computer -> Memory -> Memory
jumpCharactor computer memory =
    if computer.mouse.click == True then
        if memory.jumpcount < 2 then
            { memory
                | jumpcount = memory.jumpcount + 1
                , ymove = memory.jumppower
            }

        else
            memory

    else
        memory


gravity : Memory -> Memory
gravity memory =
    { memory
        | ymove = memory.ymove - memory.g
    }


moveCharactor : Memory -> Memory
moveCharactor memory =
    { memory
        | x = memory.x + memory.xmove
        , y = memory.y + memory.ymove
        , angle =
            (if memory.xmove > 0 then
                memory.angle - 5

             else
                memory.angle + 5
            )
                |> remainderBy 360
    }


isHit : ( Float, Float ) -> Memory -> Memory
isHit ( blockX, blockY ) memory =
    if
        abs (memory.x - blockX)
            < sumWidth memory
            && abs (memory.y - blockY)
            < sumWidth memory
    then
        if memory.xmove > 0 then
            if memory.ymove > 0 then
                case hitPoint memory blockX blockY of
                    Top ->
                        { memory
                            | y = blockY + sumWidth memory
                            , ymove = -1 * memory.ymove
                        }

                    Bottom ->
                        { memory
                            | y = blockY - sumWidth memory
                            , ymove = -1 * memory.ymove
                        }

                    Left ->
                        { memory
                            | x = blockX - sumWidth memory
                            , xmove = -1 * memory.xmove
                        }

                    Right ->
                        { memory
                            | x = blockX + sumWidth memory
                            , xmove = -1 * memory.xmove
                        }

            else if memory.ymove < 0 then
                case hitPoint memory blockX blockY of
                    Top ->
                        { memory
                            | y = blockY + sumWidth memory
                            , ymove = 0
                            , jumpcount = 0
                        }

                    Bottom ->
                        memory

                    Left ->
                        { memory
                            | x = blockX - sumWidth memory
                            , xmove = -1 * memory.xmove
                        }

                    Right ->
                        { memory
                            | x = blockX + sumWidth memory
                            , xmove = -1 * memory.xmove
                        }

            else
                case hitPoint memory blockX blockY of
                    Top ->
                        memory

                    Bottom ->
                        memory

                    Left ->
                        { memory
                            | x = blockX - sumWidth memory
                            , xmove = -1 * memory.xmove
                        }

                    Right ->
                        memory

        else if memory.ymove > 0 then
            case hitPoint memory blockX blockY of
                Top ->
                    { memory
                        | y = blockY + sumWidth memory
                        , ymove = -1 * memory.ymove
                    }

                Bottom ->
                    { memory
                        | y = blockY - sumWidth memory
                        , ymove = -1 * memory.ymove
                    }

                Left ->
                    { memory
                        | x = blockX - sumWidth memory
                        , xmove = -1 * memory.xmove
                    }

                Right ->
                    { memory
                        | x = blockX + sumWidth memory
                        , xmove = -1 * memory.xmove
                    }

        else if memory.ymove < 0 then
            case hitPoint memory blockX blockY of
                Top ->
                    { memory
                        | y = blockY + sumWidth memory
                        , ymove = 0
                        , jumpcount = 0
                    }

                Bottom ->
                    memory

                Left ->
                    { memory
                        | x = blockX - sumWidth memory
                        , xmove = -1 * memory.xmove
                    }

                Right ->
                    { memory
                        | x = blockX + sumWidth memory
                        , xmove = -1 * memory.xmove
                    }

        else
            case hitPoint memory blockX blockY of
                Top ->
                    memory

                Bottom ->
                    memory

                Left ->
                    memory

                Right ->
                    { memory
                        | x = blockX + sumWidth memory
                        , xmove = -1 * memory.xmove
                    }

    else
        memory


hitPoint : Memory -> Float -> Float -> HitPoint
hitPoint memory blockX blockY =
    if abs (memory.x - blockX) < abs (memory.y - blockY) then
        if memory.y < blockY then
            Bottom

        else
            Top

    else if memory.x < blockX then
        Left

    else
        Right


sumWidth : Memory -> Float
sumWidth memory =
    memory.blockWidth / 2 + memory.charactorWidth / 2


checkGround : Memory -> Memory
checkGround memory =
    if memory.y < Tuple.second startPosition then
        { memory
            | y = Tuple.second startPosition
            , ymove = 0
            , jumpcount = 0
        }

    else
        memory


checkHitStartWall : Memory -> Memory
checkHitStartWall memory =
    if memory.x < Tuple.first startPosition then
        { memory
            | x = Tuple.first startPosition
            , xmove = -1 * memory.xmove
        }

    else
        memory


checkGoal : Memory -> Memory
checkGoal memory =
    if memory.x > goalPosition then
        { memory
            | scene = Goal
        }

    else
        memory
