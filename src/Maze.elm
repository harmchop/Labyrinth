module Maze exposing (main)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color exposing (Color)
import Dict exposing (Dict)
import Random exposing (Generator)
import Array
import Debug exposing (log)
import Time
import Browser
import Html.Events exposing (onInput)
import Html.Attributes exposing (style, min, max, type_, value)
import Html exposing (Html, text)
import Material.Slider as Slider
import Material.Typography as Typography
import Material.Button as Button
import Material.Select as Select
import Material.Select.Option as SelectOption
import Delay exposing (after, TimeUnit(..))
import ColorPicker


addLink p1 p2 ns = 
    let
        nsP1 = (case Dict.get p1 ns of
            Just a -> a 
            Nothing -> [])
        nsP2 = (case Dict.get p2 ns of
            Just a -> a 
            Nothing -> [])
        nsP1N = p2 :: nsP1
        nsP2N = p1 :: nsP2
        --lg = (log "(p1, p2)" (p1, p2))
    in Dict.insert p1 nsP1N ns 
    |> Dict.insert p2 nsP2N

links ns p1 = (case Dict.get p1 ns of
        Just a -> a 
        Nothing -> [])

isLinked ns p1 p2 =
    let
        lns = (links ns p1)
        res = List.member p2 <| lns
    in res

-- draws a line if not isN 
drawLineIfnt isN p =
    if isN then
        moveTo p
    else
        lineTo p

southNeighbor (x, y) =
    (x,y+1)

eastNeighbor (x, y) = 
    (x+1, y)

northNeighbor (x, y) =
    (x, y-1)

westNeighbor (x, y) =
    (x-1, y)

cell : Model -> Float -> Float -> Renderable
cell model i j = 
    let
        w = model.cellSize
        ns = model.neighborhood
        x1 = 4 + i * w
        x2 = x1 + w
        y1 = 4 + j * w
        y2 = y1 + w
    in
        shapes
            [ stroke <| Color.rgb255 0 0 0
            , transform [translate 0 0]
            , lineWidth model.wallWidth ]
            [ path ( x1, y1 )
                [ drawLineIfnt (isLinked ns (i,j) (northNeighbor (i,j))) (x2,y1)
                , drawLineIfnt (isLinked ns (i,j) (eastNeighbor (i,j))) (x2,y2)
                , drawLineIfnt (isLinked ns (i,j) (southNeighbor (i,j))) (x1,y2)
                , drawLineIfnt (isLinked ns (i,j) (westNeighbor (i,j))) (x1, y1 - model.wallWidth / 2)
                ]
            ] 

ground : Model -> Point -> Int -> List Renderable -> List Renderable
ground model (i,j) d gs = 
    let
        w = model.cellSize
        x = 4 + i * w
        y = 4 + j * w
        rgba = Color.toRgba model.color
    in
        (shapes [ fill ( Color.fromRgba 
            { red = rgba.red
            , green = rgba.green
            , blue = rgba.blue
            , alpha = (toFloat d) / (toFloat model.maxDist)
            } ) ] 
        [ rect (x,y) w w ]) 
        :: gs

northEastNeighbors width p1 =
    let
        (i, j) = p1
        ns = []
        ns2 = if i < width then (eastNeighbor p1) :: ns else ns
        ns3 = if j > 0 then (northNeighbor p1) :: ns2 else ns2
    in 
        ns3

generatorLinkArgs height width p1 =
    let
        ns = Array.fromList (northEastNeighbors width p1)
    in 
        Random.int 0 ((Array.length ns) - 1)

addDistance (p, d) dist = Dict.insert p (d + 1) dist

distances : Model -> List Point -> Model
distances model frontier =
    let 
        dist p = case Dict.get p model.distances of
            Just d -> d
            Nothing -> 0
        newFrontDist =
            List.concatMap (\p ->
                    List.filterMap (\l -> 
                            if Dict.member l model.distances 
                            then Nothing
                            else Just (l, dist p)
                        )
                        (links model.neighborhood p)
                )
                (frontier) 
        newDist = List.foldl addDistance model.distances newFrontDist
        newFront = List.map (\(p, d) -> p) newFrontDist
    in 
        if List.length frontier > 0 then
            distances { model | distances = newDist } newFront
        else
            model

{- Main program loop -}

type alias Model =
    { animating : Bool
    , width : Int
    , height : Int
    , linkQueue: List (Point, Point)
    , neighborhood : Neighborhood
    , canvasSize : Int
    , cellSize : Float
    , animationDelay : Float
    , wallWidth : Float
    , colorPicker: ColorPicker.State
    , color : Color
    , distances : Dict (Float, Float) Int
    , maxDist : Int}

type Msg
    = Reset
    | AddLink Point Point
    | Tick
    | CanvasSize Float
    | CellSize Float
    | AnimationDelay Float
    | Animate
    | Color
    | Algorithm String
    | WallWidth Float
    | ColorPickerMsg ColorPicker.Msg
    
type alias Neighborhood =
    Dict (Float, Float) (List (Float, Float))

view model = 
    let 
        res = Html.div [ Typography.typography ]
            [ Html.div 
                    [ Typography.typography
                    , style "display" "block"
                    , style "width" "200px"
                    , style "float" "left"
                    , style "margin-right" "20px"]
                    [ Button.raised (Button.config 
                            |> Button.setOnClick Animate 
                            |> Button.setAttributes [ style "width" "200px", style "margin-bottom" "10px"]) 
                        "Generate"
                    , Button.raised (Button.config 
                            |> Button.setOnClick Color |> Button.setDisabled (model.animating) 
                            |> Button.setAttributes [ style "width" "200px", style "margin-bottom" "10px"]) 
                        "Color"
                    , Html.h4 [ Typography.subtitle1 ] [ Html.text "Algorithm" ]
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Generation Algorithm" ]
                    , Html.select [ style "width" "100%"]
                        [ Html.option [ value "BinaryTree" ] [ Html.text "Binary Tree"]
                        , Html.option [ value "Sidewinder" ] [ Html.text "Sidewinder"]
                        , Html.option [ value "AldousBroder" ] [ Html.text "Aldous Broder"]
                        , Html.option [ value "Wilsons" ] [ Html.text "Wilson's"]
                        , Html.option [ value "HuntAndKill" ] [ Html.text "Hunt-and-Kill"]
                        , Html.option [ value "RecursiveBacktracking" ] [ Html.text "Recursive Backtracking"]
                        , Html.option [ value "Prims" ] [ Html.text "Prim's"]
                        ]
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Coloring Algorithm" ]
                    , Html.select [ style "width" "100%"]
                        [ Html.option [ value "DFS" ] [ Html.text "DFS"]
                        , Html.option [ value "BFS" ] [ Html.text "BFS"]
                        , Html.option [ value "Dijkstra" ] [ Html.text "Dijkstra"]
                        ]
                    , Html.h4 [ Typography.subtitle1 ] [ Html.text "Display" ]
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Animation Speed" ]
                    , Html.input 
                        [ style "width" "100%"
                        , type_ "range"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "200"
                        , value (String.fromInt (200 - round model.animationDelay))
                        , onInput (\x -> AnimationDelay (
                            case (String.toFloat x) of
                                Just f -> 200 - f 
                                Nothing -> model.animationDelay)   
                        )] 
                        []
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Canvas Size" ]
                    , Html.input 
                        [ style "width" "100%"
                        , type_ "range"
                        , Html.Attributes.min "300"
                        , Html.Attributes.max "1000"
                        , value (String.fromInt model.canvasSize)
                        , onInput (\x -> CanvasSize (
                            case (String.toFloat x) of
                                Just f -> f
                                Nothing -> toFloat model.canvasSize)   
                        )] 
                        []
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Color" ]
                    , ColorPicker.view model.color model.colorPicker
                        |> Html.map ColorPickerMsg
                    , Html.h4 [ Typography.subtitle1 ] [ Html.text "Cells" ]
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Cell Size" ]
                    , Html.input 
                        [ style "width" "100%"
                        , type_ "range"
                        , Html.Attributes.min "10"
                        , Html.Attributes.max "200"
                        , value (String.fromFloat model.cellSize)
                        , onInput (\x -> CellSize (
                            case (String.toFloat x) of
                                Just f -> f
                                Nothing -> model.cellSize)   
                        )] 
                        []
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Wall Width" ]
                    , Html.input 
                        [ style "width" "100%"
                        , type_ "range"
                        , Html.Attributes.min "1"
                        , Html.Attributes.max "10"
                        , value (String.fromFloat model.wallWidth)
                        , onInput (\x -> WallWidth (
                            case (String.toFloat x) of
                                Just f -> f
                                Nothing -> model.wallWidth)   
                        )] 
                        []
                ]
                , Canvas.toHtml ( model.canvasSize, model.canvasSize ) [ style "float" "left" ]
                ((clear ( 0, 0 ) (toFloat model.canvasSize) (toFloat model.canvasSize)) ::
                (Dict.foldl (ground model) [] model.distances) ++
                (List.range 0 model.height
                |> List.map toFloat
                |> List.concatMap (\j -> 
                    (List.map ((|>) j)
                    (List.range 0 model.width
                        |> List.map toFloat
                        |> List.map (cell model))))))
            ]
    in res

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Reset ->
            let 
                width = toFloat ((model.canvasSize - 10) // (round model.cellSize) - 1)
                height = toFloat ((model.canvasSize - 10) // (round model.cellSize) - 1)
                cells = List.range 0 (round height)
                    |> List.map toFloat
                    |> List.concatMap 
                        (\j -> (List.map ((|>) j) 
                            (List.range 0 (round width)
                            |> List.map toFloat
                            |> List.map (\i k -> (i, k)))))
                batch = List.map 
                    (\(i, j) -> 
                        let
                            p1 = (i, j)
                        in 
                        Random.generate (\k -> (AddLink p1 
                            (case Array.get k (Array.fromList (northEastNeighbors width p1)) of
                                Just p2 -> p2
                                Nothing -> (-2,-2)
                            )
                            )) 
                            (generatorLinkArgs height width p1)
                            
                    ) cells
            in ({ model 
                    | animating = True
                    , linkQueue = []
                    , distances = Dict.empty
                    , maxDist = 0
                    , neighborhood = Dict.empty
                    , width = (round width)
                    , height = (round height)
                },
                Cmd.batch batch)
        AddLink p1 p2 ->
            ({ model 
                | linkQueue = (p1, p2) :: model.linkQueue }
            , Cmd.none)
        Tick ->
            (case model.linkQueue of
              [] -> { model | animating = False }
              (p1,p2) :: rest -> { model | linkQueue = rest, neighborhood = addLink p1 p2 model.neighborhood }
            , Cmd.none)
        AnimationDelay f ->
            ({ model | animationDelay = f}
            , after 0 Millisecond Reset)
        WallWidth f ->
            ({ model | wallWidth = f}
            , after 0 Millisecond Reset)
        CellSize f ->
            ({ model | cellSize = f}
            , after 0 Millisecond Reset)
        CanvasSize f ->
            ({ model | canvasSize = (round f)
            , width = ((model.canvasSize - 10) // (round model.cellSize) - 1)
            , height = ((model.canvasSize - 10) // (round model.cellSize) - 1)}
            , after 0 Millisecond Reset)
        Animate ->
            (model
            , after 0 Millisecond Reset)
        Color ->
            let
                modelN = distances model [(0,0)]
                maxDist = Dict.foldl (\_ d m ->
                    if d > m then d else m) 0 (modelN.distances)
                lg = log "maxDist" maxDist
            in
                ({ modelN | maxDist = maxDist }
                , Cmd.none)
        Algorithm s ->
            (model
            , after 0 Millisecond Reset)
        ColorPickerMsg m ->
            let 
                (cp, color) = ColorPicker.update m model.color model.colorPicker
            in 
                ({ model
                    | colorPicker = cp
                    , color = color |> Maybe.withDefault model.color
                }
                , Cmd.none)

init : () -> ( Model, Cmd Msg )
init () =
    ({ animating = True
    , linkQueue = []
    , width = 0
    , height = 0
    , neighborhood = Dict.empty 
    , cellSize = 20
    , canvasSize = 700
    , animationDelay = 1
    , wallWidth = 1
    , colorPicker = ColorPicker.empty
    , color = Color.red
    , distances = Dict.fromList [((0,0), 0)]
    , maxDist = 0}
    , after 0 Millisecond Reset)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> update msg model
        , subscriptions = \model -> Time.every model.animationDelay (always Tick)
        }