module Maze exposing (main)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color exposing (Color)
import Dict exposing (Dict)
import Random exposing (Generator, Seed)
import Array exposing (Array)
import Array.Extra exposing (removeWhen)
import List.Extra exposing (elemIndex)
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
import Time


addLink : Point -> Point -> Neighborhood -> Neighborhood
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

links : Neighborhood -> Point -> List Point
links ns p1 = (case Dict.get p1 ns of
        Just a -> a 
        Nothing -> [])

isLinked : Neighborhood -> Point -> Point -> Bool
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

southNeighbor : Point -> Point
southNeighbor (x, y) =
    (x,y+1)

eastNeighbor : Point -> Point
eastNeighbor (x, y) = 
    (x+1, y)

northNeighbor : Point -> Point
northNeighbor (x, y) =
    (x, y-1)

westNeighbor : Point -> Point
westNeighbor (x, y) =
    (x-1, y)

atEasternBoundary : Model -> Point -> Bool
atEasternBoundary model (x, y) =
    let
        width = toFloat model.width
        (i, j) = eastNeighbor (x, y)
    in i > width

atNorthernBoundary : Model -> Point -> Bool
atNorthernBoundary model (x, y) =
    let
        (i, j) = northNeighbor (x, y)
    in j < 0

northEastNeighbors : Model -> Point -> List Point
northEastNeighbors model p =
    let
        width = toFloat ((model.canvasSize - 10) // (round model.cellSize) - 1)
        (i, j) = p
        ns = []
        ns2 = if i < width then (eastNeighbor p) :: ns else ns
        ns3 = if j > 0 then (northNeighbor p) :: ns2 else ns2
    in 
        ns3

neighbors : Model -> Point -> List Point
neighbors model p = 
    let
        width = toFloat model.width
        height = toFloat model.height
        (i, j) = p
        ns = []
        ns2 = if i < width then (eastNeighbor p) :: ns else ns
        ns3 = if j > 0 then (northNeighbor p) :: ns2 else ns2
        ns4 = if j < height then (southNeighbor p) :: ns3 else ns3
        ns5 = if i > 0 then (westNeighbor p) :: ns4 else ns4
    in 
        ns5

walls : Model -> Float -> Float -> Renderable
walls model i j = 
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

shortestPath : Model -> Point -> List Point
shortestPath model end = 
    let linked = links model.neighborhood end
        ds = List.map (\p -> 
                case (Dict.get p model.distances) of 
                    Just d -> (toFloat d, p) 
                    Nothing -> (1/0, p)
            ) linked
        (dist, next) = List.foldl (\(d1, p1) (d2, p2) ->
                if d1 < d2 || p1 == (0,0) then
                    (d1, p1)
                else
                    (d2, p2))
                (1/0, (0,0)) 
            (ds)
        path = if end == (0,0) then [(0,0)] else end :: (shortestPath model next)
    in path

generatorLinkArgs : Model -> Point -> Generator Int
generatorLinkArgs model p =
    let
        ns = Array.fromList (northEastNeighbors model p)
    in 
        Random.int 0 ((Array.length ns) - 1)

generatorABLinkArgs : Model -> Point -> Generator Int
generatorABLinkArgs model p = 
    let 
        width = toFloat ((model.canvasSize - 10) // (round model.cellSize) - 1)
        height = toFloat ((model.canvasSize - 10) // (round model.cellSize) - 1)
        ns =  Array.fromList (neighbors model p)
    in 
        Random.int 0 ((Array.length ns) - 1)

{- Maze generation algorithms -}

binaryTree : Model -> (Model, List (Cmd Msg))
binaryTree model = 
    let 
        width = model.width
        height = model.height
        cells = List.range 0 height
            |> List.map toFloat
            |> List.concatMap 
                (\j -> (List.map ((|>) j) 
                    (List.range 0 width
                    |> List.map toFloat
                    |> List.map (\i k -> (i, k)))))
        batch = List.map 
            (\(i, j) -> 
                let
                    p1 = (i, j)
                in 
                Random.generate (\k -> (AddLink p1 
                    (case Array.get k (Array.fromList (northEastNeighbors model p1)) of
                        Just p2 -> p2
                        Nothing -> (-2,-2)
                    )
                    )) 
                    (generatorLinkArgs model p1)       
            ) cells
    in (model, batch)

sidewinder : Model -> (Model, List (Cmd Msg))
sidewinder model =
    let
        height = model.height
        rows = List.range 0 height
            |> List.map toFloat
        modelN = List.foldl 
            (\j m -> sidewinderH m (Array.empty) 0 j)
            model rows
    in (modelN, [])

sidewinderH : Model -> Array Point -> Float -> Float -> Model
sidewinderH model run i j = 
    let 
        width = toFloat model.width
        seed = model.seed
        runN = Array.push (i,j) run
        (k, seedN) = Random.step (Random.int 0 2) seed
        shouldCloseOut = atEasternBoundary model (i,j) || 
            (not (atNorthernBoundary model (i,j)) && (k == 0))
        (m, seedNN) = Random.step 
            (Random.int 0 ((Array.length runN) - 1)) seedN
        member = case Array.get m runN of
            Just mem -> mem
            Nothing -> (-2,-2)
        modelN = if shouldCloseOut
            then
                let 
                    linkQ = if (not (atNorthernBoundary model member)) 
                        then
                            (member, northNeighbor member) :: model.linkQueue
                        else
                            model.linkQueue
                    in { model 
                        | linkQueue = linkQ }
            else { model
                    | linkQueue  = ((i,j), eastNeighbor (i, j)) :: model.linkQueue}
        runNN = if shouldCloseOut
            then Array.empty
            else runN
        modelNN = { modelN
                    | seed = seedNN}
    in if i == width 
        then modelNN
        else sidewinderH modelNN runNN (i+1) j

aldousBroder : Model -> (Model, List (Cmd Msg))
aldousBroder model =
    let 
        seed = model.seed
        width = model.width
        height = model.height
        point = Random.pair (Random.int 0 width) (Random.int 0 height)
        ((i, j), seedN) = Random.step point seed
        p1 = (toFloat i, toFloat j)
        modelN = { model | seed = seedN }
        batch = Random.generate 
                    (\k -> ABAddLink p1 
                        (case Array.get k (Array.fromList (neighbors modelN p1)) of
                            Just p2 -> p2
                            Nothing -> (-2,-2)
                        )
                    ) 
                    (generatorABLinkArgs modelN p1)           
    in (modelN, [batch])

wilsons : Model -> (Model, List (Cmd Msg))
wilsons model =
    let
        height = model.height
        width = model.width
        unvis = List.range 0 height
            |> List.map toFloat
            |> List.concatMap 
                (\j -> (List.map ((|>) j) 
                    (List.range 0 width
                    |> List.map toFloat
                    |> List.map (\i k -> (i, k)))))
            |> Array.fromList
        (cellIdx, seedN) = Random.step (Random.int 0 ((Array.length unvis) - 1)) model.seed
        cell = case Array.get cellIdx unvis of
            Just a -> a
            Nothing -> (-2,-2)
        unvisN = removeWhen ((==) cell) unvis
        modelN = { model | seed = seedN}
        modelNN = wilsonsH modelN unvisN
    in (modelNN, [])

wilsonsH model unvis = 
    if Array.length unvis > 0 then
        let
            (cellIdx, seedN) = Random.step (Random.int 0 ((Array.length unvis) - 1)) model.seed
            cell = case Array.get cellIdx unvis of
                Just a -> a
                Nothing -> (-2,-2)
            -- lg = log "cell" cell
            modelN = { model | seed = seedN }
            (modelNN, path) = wilsonsBuildPath modelN unvis (Array.fromList [cell]) cell
            modelNNN = wilsonsAddLinks modelNN path
            unvisN = Array.foldl 
                (\p -> Array.Extra.removeWhen ((==) p)) 
                unvis 
                (Array.slice 0 (Array.length path - 1) path)
                -- path
        in
            wilsonsH modelNNN unvisN
    else
        model

wilsonsBuildPath model unvis path cell =
    if (Array.filter ((==) cell) unvis) /= Array.empty then
        let
            ns = Array.fromList (neighbors model cell)
            (cellIdx, seedN) = Random.step (Random.int 0 ((Array.length ns) - 1)) model.seed
            n = case Array.get cellIdx ns of
                    Just a -> a
                    Nothing -> (-2,-2)
            pathL = Array.toList path
            position = case elemIndex n pathL of
                Just k -> k
                Nothing -> -1
            pathN = if position /= -1 
                then
                    Array.slice 0 (position + 1) path
                else
                    Array.push n path
            modelN = { model | seed = seedN}
        in wilsonsBuildPath modelN unvis pathN n
    else
        (model, path)
    
wilsonsAddLinks model path =
    let
        ls = Array.Extra.zip 
            (Array.slice 0 (Array.length path - 1) path) 
            (Array.slice 1 (Array.length path) path)
        linkQ = Array.foldl (::) model.linkQueue ls
    in 
        {model | linkQueue = linkQ}        

huntAndKill model =
    let
        height = model.height
        width = model.width
        gridL = List.range 0 height
            |> List.map toFloat
            |> List.concatMap 
                (\j -> (List.map ((|>) j) 
                    (List.range 0 width
                    |> List.map toFloat
                    |> List.map (\i k -> (i, k)))))
        grid = Array.fromList gridL
        (cellIdx, seedN) = Random.step (Random.int 0 ((Array.length grid) - 1)) model.seed
        current = Array.get cellIdx grid
        modelN = { model | seed = seedN }
    in
        (huntAndKillH modelN gridL current, [])

huntAndKillH model grid current =
    if current /= Nothing then
        let 
            cell = case current of
                Just a -> a
                Nothing -> (-2,-2)
            unvisited_neighbors = List.filter 
                (\c -> links model.neighborhood c == [])
                (neighbors model cell)
            -- un_neighbors = []
            (modelN, currentN) = if unvisited_neighbors /= [] 
            -- (modelN, currentN) = if un_neighbors /= [] 
                then
                    let
                        ns = Array.fromList unvisited_neighbors
                        (cellIdx, seedN) = Random.step 
                            (Random.int 0 ((Array.length ns) - 1)) 
                            model.seed
                        n = case Array.get cellIdx ns of
                                Just a -> a
                                Nothing -> (-2,-2) 
                        modelNN = { model 
                            | linkQueue = (cell, n) :: model.linkQueue 
                            , seed = seedN }
                    in (modelNN, Just n)
                else
                    hunt model grid Nothing
            lg1 = log "currentN" currentN
        -- in huntAndKillH modelN grid currentN
        in model
    else
        model

hunt model grid current =
    case grid of 
        cell :: rest -> 
            let
                visited_neighbors = List.filter 
                    (\c -> links model.neighborhood c /= [])
                    (neighbors model cell)
                ns = Array.fromList visited_neighbors
                (cellIdx, seedN) = Random.step 
                    (Random.int 0 ((Array.length ns) - 1)) 
                    model.seed
                n = case Array.get cellIdx ns of
                        Just a -> a
                        Nothing -> (-2,-2) 
                isCandidate = (links model.neighborhood cell == []) && (visited_neighbors /= [])
                modelN = { model | seed = seedN }
                lg = log "hunt(cell, visited_neighbors, isCandidate)" (cell, visited_neighbors, isCandidate)
            in
                if isCandidate then
                    ({ modelN 
                        | linkQueue = (cell, n) :: modelN.linkQueue }
                    , Just cell)
                else 
                    hunt modelN rest current
        [] -> (model, current)
        
recursiveBacktracker model =
    let
        height = model.height
        width = model.width
        grid = List.range 0 height
            |> List.map toFloat
            |> List.concatMap 
                (\j -> (List.map ((|>) j) 
                    (List.range 0 width
                    |> List.map toFloat
                    |> List.map (\i k -> (i, k)))))
            |> Array.fromList
        (cellIdx, seedN) = Random.step (Random.int 0 ((Array.length grid) - 1)) model.seed
        modelN = { model
            | seed = seedN }
        start = case Array.get cellIdx grid of
            Just a -> a
            Nothing -> (-2,-2)
        stack = Array.fromList [start]
    in (recursiveBacktrackerH modelN stack, [])

recursiveBacktrackerH model stack =
    if stack /= Array.empty then
        let
            cell = case Array.get 0 (Array.slice 0 -1 stack) of
                Just a -> a
                Nothing -> (-2,-2)
            unvisited_ns = List.filter 
                (\c -> links model.neighborhood c == [])
                (neighbors model cell) 
                |> Array.fromList 
            (modelN, stackN) =
                if unvisited_ns == Array.empty then
                    (model, Array.Extra.pop stack)
                else
                    let
                        ns = unvisited_ns
                        (cellIdx, seedN) = Random.step 
                            (Random.int 0 ((Array.length ns) - 1)) 
                            model.seed
                        n = case Array.get cellIdx unvisited_ns of
                                Just a -> a
                                Nothing -> (-2,-2)
                        modelNN = { model 
                            | linkQueue = (cell, n) :: model.linkQueue
                            , seed = seedN }
                        stackNN = Array.push n stack
                    in (modelNN, stackNN)
            in recursiveBacktrackerH modelN stackN
    else model


{- Main program loop -}

type alias Model =
    { animating : Bool
    , width : Int
    , height : Int
    , canvasSize : Int
    , cellSize : Float
    , animationDelay : Float
    , wallWidth : Float
    , colorPicker: ColorPicker.State
    , color : Color
    , linkQueue: List (Point, Point)
    , neighborhood : Neighborhood
    , neighborQueue : Neighborhood
    , unvisited : Int
    , distances : Dict Point Int
    , maxDist : Int
    , path : List Point
    , algorithm : String
    , seed : Seed }

type Msg
    = Reset
    | ABAddLink Point Point
    | AddLink Point Point
    | Tick
    | CanvasSize Float
    | CellSize Float
    | AnimationDelay Float
    | Animate
    | Solve
    | Algorithm String
    | WallWidth Float
    | ColorPickerMsg ColorPicker.Msg
    
type alias Neighborhood =
    Dict Point (List Point)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Reset ->
            let 
                width = ((model.canvasSize - 10) // (round model.cellSize) - 1)
                height = ((model.canvasSize - 10) // (round model.cellSize) - 1)
                cells = List.range 0 height
                    |> List.map toFloat
                    |> List.concatMap 
                        (\j -> (List.map ((|>) j) 
                            (List.range 0 width
                            |> List.map toFloat
                            |> List.map (\i k -> (i, k)))))
                modelN = { model 
                        | animating = True
                        , linkQueue = []
                        , distances = Dict.empty
                        , maxDist = 0
                        , path = []
                        , neighborhood = Dict.empty
                        , neighborQueue = Dict.empty
                        , width = width
                        , height = height
                        , unvisited = (List.length cells) - 1
                    }
                (modelNN, batch) = case model.algorithm of
                    "BinaryTree" -> binaryTree modelN
                    "AldousBroder" -> aldousBroder modelN
                    "Sidewinder" -> sidewinder modelN
                    "Wilsons" -> wilsons modelN
                    "HuntAndKill" -> huntAndKill modelN
                    "RecursiveBacktracker" -> recursiveBacktracker modelN
                    _ -> binaryTree modelN
            in (modelNN,
                Cmd.batch batch)
        ABAddLink p1 p2 ->
            let
                modelN = if links model.neighborQueue p2 == [] 
                    then
                        { model 
                            | neighborQueue = addLink p1 p2 model.neighborQueue
                            , linkQueue = (p1, p2) :: model.linkQueue
                            , unvisited = model.unvisited - 1 }
                    else
                        model
                next = if modelN.unvisited > 0
                    then
                        Random.generate (\k -> (ABAddLink p2 
                            (case Array.get k (Array.fromList (neighbors model p2)) of
                                Just p3 -> p3
                                Nothing -> (-2,-2)
                            ))) 
                            (generatorABLinkArgs model p2)
                    else Cmd.none
            in
                (modelN, next)
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
        Solve ->
            let
                modelN = distances model [(0,0)]
                maxDist = Dict.foldl (\_ d m ->
                    if d > m then d else m) 0 (modelN.distances)
                path = List.reverse (shortestPath modelN (0, toFloat model.height))
            in
                ({ modelN | maxDist = maxDist, path = path}
                , Cmd.none)
        Algorithm s ->
            ({ model | algorithm = s }
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

init : { startTime : Float } -> ( Model, Cmd Msg )
init { startTime } =
    let
        model = 
            { animating = True
            , width = 0
            , height = 0
            , cellSize = 50
            , canvasSize = 500
            , animationDelay = 1
            , wallWidth = 1
            , colorPicker = ColorPicker.empty
            , color = Color.red
            , distances = Dict.fromList [((0,0), 0)]
            , linkQueue = []
            , neighborhood = Dict.empty
            , neighborQueue = Dict.empty 
            , unvisited = 0
            , maxDist = 0
            , path = []
            , seed = Random.initialSeed <| round startTime
            , algorithm = "Wilsons"}
    in
    ( model
    , after 0 Millisecond Reset)


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
                            |> Button.setOnClick Solve |> Button.setDisabled (model.animating) 
                            |> Button.setAttributes [ style "width" "200px", style "margin-bottom" "10px"]) 
                        "Solve"
                    , Html.h4 [ Typography.subtitle1 ] [ Html.text "Algorithm" ]
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Generation Algorithm" ]
                    , Html.select [ style "width" "100%", onInput Algorithm, value model.algorithm]
                        [ Html.option [ value "BinaryTree" ] [ Html.text "Binary Tree"]
                        , Html.option [ value "Sidewinder" ] [ Html.text "Sidewinder"]
                        , Html.option [ value "AldousBroder" ] [ Html.text "Aldous Broder"]
                        , Html.option [ value "Wilsons" ] [ Html.text "Wilson's"]
                        , Html.option [ value "HuntAndKill" ] [ Html.text "Hunt-and-Kill"]
                        , Html.option [ value "RecursiveBacktracker" ] [ Html.text "Recursive Backtracking"]
                        , Html.option [ value "Prims" ] [ Html.text "Prim's"]
                        ]
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Solution Algorithm" ]
                    , Html.select [ style "width" "100%"]
                        [ Html.option [ value "Dijkstra" ] [ Html.text "Dijkstra"]
                        , Html.option [ value "DFS" ] [ Html.text "DFS"]
                        , Html.option [ value "BFS" ] [ Html.text "BFS"]
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
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Distance Color" ]
                    , ColorPicker.view model.color model.colorPicker
                        |> Html.map ColorPickerMsg
                    , Html.h4 [ Typography.subtitle1 ] [ Html.text "Cells" ]
                    , Html.div [ Typography.subtitle2 ] [ Html.text "Cell Size" ]
                    , Html.input 
                        [ style "width" "100%"
                        , type_ "range"
                        , Html.Attributes.min "50"
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
                (if model.path == [] then [] else [shapes
                    [ stroke <| Color.blue
                    , transform [translate 0 0]
                    , lineWidth model.wallWidth ]
                    [ path ( 4 + model.cellSize / 2, 4 + model.cellSize / 2 )
                        (List.map (
                            \(x,y) -> 
                                lineTo 
                                    ((4 + x * model.cellSize + (model.cellSize / 2))
                                    , (4 + y * model.cellSize + (model.cellSize / 2))))
                                 model.path)
                    ]
                ]) ++
                (List.range 0 model.height
                |> List.map toFloat
                |> List.concatMap (\j -> 
                    (List.map ((|>) j)
                    (List.range 0 model.width
                        |> List.map toFloat
                        |> List.map (walls model))))))
            ]
    in res

main : Program { startTime : Float} Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> update msg model
        , subscriptions = \model -> Time.every model.animationDelay (always Tick)
        }