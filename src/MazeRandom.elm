module Maze exposing (main)

import Array
import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
import Color
import Debug exposing (log)
import Delay exposing (TimeUnit(..), after)
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes exposing (max, min, style, type_, value)
import Html.Events exposing (onInput)
import Material.Button as Button
import Material.Select as Select
import Material.Select.Option as SelectOption
import Material.Slider as Slider
import Material.Typography as Typography
import Random exposing (Generator)
import Random.Extra
import Time


addLink p1 p2 ns =
    let
        nsP1 =
            case Dict.get p1 ns of
                Just a ->
                    a

                Nothing ->
                    []

        nsP2 =
            case Dict.get p2 ns of
                Just a ->
                    a

                Nothing ->
                    []

        nsP1N =
            p2 :: nsP1

        nsP2N =
            p1 :: nsP2

        --lg = (log "(p1, p2)" (p1, p2))
    in
    Dict.insert p1 nsP1N ns
        |> Dict.insert p2 nsP2N


links ns p1 =
    case Dict.get p1 ns of
        Just a ->
            a

        Nothing ->
            []


isLinked ns p1 p2 =
    let
        lns =
            links ns p1

        res =
            List.member p2 <| lns
    in
    res



-- draws a line if not isN


drawLineIfnt isN p =
    if isN then
        moveTo p

    else
        lineTo p


southNeighbor ( x, y ) =
    ( x, y + 1 )


eastNeighbor ( x, y ) =
    ( x + 1, y )


northNeighbor ( x, y ) =
    ( x, y - 1 )


westNeighbor ( x, y ) =
    ( x - 1, y )


cell : Model -> Float -> Float -> Renderable
cell model i j =
    let
        w =
            model.cellSize

        ns =
            model.neighborhood

        x1 =
            4 + i * w

        x2 =
            x1 + w

        y1 =
            4 + j * w

        y2 =
            y1 + w
    in
    shapes
        [ stroke <| Color.rgb255 0 0 0
        , transform [ translate 0 0 ]
        , lineWidth model.wallWidth
        ]
        [ path ( x1, y1 )
            [ drawLineIfnt (isLinked ns ( i, j ) (northNeighbor ( i, j ))) ( x2, y1 )
            , drawLineIfnt (isLinked ns ( i, j ) (eastNeighbor ( i, j ))) ( x2, y2 )
            , drawLineIfnt (isLinked ns ( i, j ) (southNeighbor ( i, j ))) ( x1, y2 )
            , drawLineIfnt (isLinked ns ( i, j ) (westNeighbor ( i, j ))) ( x1, y1 )
            ]
        ]


northEastNeighbors width p1 =
    let
        ( i, j ) =
            p1

        ns =
            []

        ns2 =
            if i < width then
                eastNeighbor p1 :: ns

            else
                ns

        ns3 =
            if j > 0 then
                northNeighbor p1 :: ns2

            else
                ns2
    in
    ns3


generatorLinkArgs height width p1 =
    let
        ns =
            Array.fromList (northEastNeighbors width p1)
    in
    Random.int 0 (Array.length ns - 1)



{- Main program loop -}


type alias Model =
    { width : Int
    , height : Int
    , linkQueue : List ( Point, Point )
    , neighborhood : Neighborhood
    , canvasSize : Int
    , cellSize : Float
    , animationDelay : Float
    , wallWidth : Float
    }


type Msg
    = Reset
    | AddLinks (List ( Point, Point ))
    | Tick
    | CanvasSize Float
    | CellSize Float
    | AnimationDelay Float
    | Animate
    | Algorithm String
    | WallWidth Float


type alias Neighborhood =
    Dict ( Float, Float ) (List ( Float, Float ))


view model =
    let
        res =
            Html.div [ Typography.typography ]
                [ Canvas.toHtml ( model.canvasSize, model.canvasSize )
                    [ style "float" "left" ]
                    (clear ( 0, 0 ) (toFloat model.canvasSize) (toFloat model.canvasSize)
                        :: (List.range 0 model.height
                                |> List.map toFloat
                                |> List.concatMap
                                    (\j ->
                                        List.map ((|>) j)
                                            (List.range 0 model.width
                                                |> List.map toFloat
                                                |> List.map (cell model)
                                            )
                                    )
                           )
                    )
                , Html.div
                    [ Typography.typography
                    , style "display" "block"
                    , style "width" "200px"
                    , style "float" "left"
                    ]
                    [ Button.raised (Button.config |> Button.setOnClick Animate) "Animate"
                    , Html.h4 [ Typography.subtitle1 ] [ Html.text "Algorithm" ]
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Tiling Algorithm" ]
                    , Html.select [ style "width" "100%" ]
                        [ Html.option [ value "BinaryTree" ] [ Html.text "BinaryTree" ]
                        , Html.option [ value "Sidewinder" ] [ Html.text "Sidewinder" ]
                        ]
                    , Html.h4 [ Typography.subtitle1 ] [ Html.text "Display" ]
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Animation Delay" ]
                    , Html.input
                        [ style "width" "100%"
                        , type_ "range"
                        , Html.Attributes.min "1"
                        , Html.Attributes.max "300"
                        , value (String.fromInt (round model.animationDelay))
                        , onInput
                            (\x ->
                                AnimationDelay
                                    (case String.toFloat x of
                                        Just f ->
                                            f

                                        Nothing ->
                                            model.animationDelay
                                    )
                            )
                        ]
                        []
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Canvas Size" ]
                    , Html.input
                        [ style "width" "100%"
                        , type_ "range"
                        , Html.Attributes.min "300"
                        , Html.Attributes.max "1000"
                        , value (String.fromInt model.canvasSize)
                        , onInput
                            (\x ->
                                CanvasSize
                                    (case String.toFloat x of
                                        Just f ->
                                            f

                                        Nothing ->
                                            toFloat model.canvasSize
                                    )
                            )
                        ]
                        []
                    , Html.h4 [ Typography.subtitle1 ] [ Html.text "Cells" ]
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Cell Size" ]
                    , Html.input
                        [ style "width" "100%"
                        , type_ "range"
                        , Html.Attributes.min "10"
                        , Html.Attributes.max "200"
                        , value (String.fromFloat model.cellSize)
                        , onInput
                            (\x ->
                                CellSize
                                    (case String.toFloat x of
                                        Just f ->
                                            f

                                        Nothing ->
                                            model.cellSize
                                    )
                            )
                        ]
                        []
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Wall Width" ]
                    , Html.input
                        [ style "width" "100%"
                        , type_ "range"
                        , Html.Attributes.min "1"
                        , Html.Attributes.max "10"
                        , value (String.fromFloat model.wallWidth)
                        , onInput
                            (\x ->
                                WallWidth
                                    (case String.toFloat x of
                                        Just f ->
                                            f

                                        Nothing ->
                                            model.wallWidth
                                    )
                            )
                        ]
                        []
                    ]
                ]
    in
    res


addLinkGenerator height width p1 =
    generatorLinkArgs height width p1
        |> Random.map
            (\k ->
                ( p1
                , case Array.get k (Array.fromList (northEastNeighbors width p1)) of
                    Just p2 ->
                        p2

                    Nothing ->
                        ( -2, -2 )
                )
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            let
                width =
                    toFloat ((model.canvasSize - 10) // round model.cellSize - 1)

                height =
                    toFloat ((model.canvasSize - 10) // round model.cellSize - 1)

                cells =
                    List.range 0 (round height)
                        |> List.map toFloat
                        |> List.concatMap
                            (\j ->
                                List.map ((|>) j)
                                    (List.range 0 (round width)
                                        |> List.map toFloat
                                        |> List.map (\i k -> ( i, k ))
                                    )
                            )

                batch =
                    List.map
                        (\( i, j ) ->
                            let
                                p1 =
                                    ( i, j )
                            in
                            addLinkGenerator height width p1
                        )
                        cells
            in
            ( { model
                | linkQueue = []
                , neighborhood = Dict.empty
                , width = round width
                , height = round height
              }
            , Random.generate AddLinks (Random.Extra.combine batch)
            )

        AddLinks linklist ->
            ( { model
                | linkQueue = linklist
              }
            , Cmd.none
            )

        Tick ->
            ( case model.linkQueue of
                [] ->
                    model

                ( p1, p2 ) :: rest ->
                    { model | linkQueue = rest, neighborhood = addLink p1 p2 model.neighborhood }
            , Cmd.none
            )

        AnimationDelay f ->
            ( { model | animationDelay = f }
            , after 0 Millisecond Reset
            )

        WallWidth f ->
            ( { model | wallWidth = f }
            , after 0 Millisecond Reset
            )

        CellSize f ->
            ( { model | cellSize = f }
            , after 0 Millisecond Reset
            )

        CanvasSize f ->
            ( { model
                | canvasSize = round f
                , width = (model.canvasSize - 10) // round model.cellSize - 1
                , height = (model.canvasSize - 10) // round model.cellSize - 1
              }
            , after 0 Millisecond Reset
            )

        Animate ->
            ( model
            , after 0 Millisecond Reset
            )

        Algorithm s ->
            ( model
            , after 0 Millisecond Reset
            )


init : () -> ( Model, Cmd Msg )
init () =
    ( { linkQueue = []
      , width = 0
      , height = 0
      , neighborhood = Dict.empty
      , cellSize = 20
      , canvasSize = 400
      , animationDelay = 20
      , wallWidth = 1
      }
    , after 0 Millisecond Reset
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> update msg model
        , subscriptions = \model -> Time.every model.animationDelay (always Tick)
        }
