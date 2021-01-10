module Main exposing (main)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Browser
import Html.Attributes exposing (style)
import Dict exposing (Dict)
import Random exposing (Generator)
import Array
import Debug exposing (log)
import Time

cWidth : number
cWidth = 600

cHeight : number
cHeight = 600

cellWidth : number
cellWidth = 20

strokeWidth : number
strokeWidth = 1

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

cell : Neighborhood -> Float -> Float -> Renderable
cell ns i j = 
    let
        w = cellWidth
        x1 = 4 + i * w
        x2 = x1 + w
        y1 = 4 + j * w
        y2 = y1 + w
    in
        shapes
            [ stroke <| Color.rgb255 0 0 0
            , transform [translate 0 0]
            , lineWidth strokeWidth ]
            [ path ( x1, y1 )
                [ drawLineIfnt (isLinked ns (i,j) (northNeighbor (i,j))) (x2,y1)
                , drawLineIfnt (isLinked ns (i,j) (eastNeighbor (i,j))) (x2,y2)
                , drawLineIfnt (isLinked ns (i,j) (southNeighbor (i,j))) (x1,y2)
                , drawLineIfnt (isLinked ns (i,j) (westNeighbor (i,j))) (x1,y1)
                ]
            ]

northEastNeighbors width p1 =
    let
        (i, j) = p1
        ns = []
        ns2 = if i < width then (eastNeighbor p1) :: ns else ns
        ns3 = if j > 0 then (northNeighbor p1) :: ns2 else ns2
        lg = (log "width" (i, width))
    in 
        ns3

generatorLinkArgs height width p1 =
    let
        ns = Array.fromList (northEastNeighbors width p1)
        lg = (log "ns" (ns))
    in 
        Random.int 0 ((Array.length ns) - 1)

{- Main program loop -}

type alias Model =
    { width : Int, height : Int, linkQueue: List (Point, Point), neighborhood : Neighborhood}

type Msg
    = AddLink Point Point
    | Tick
    
type alias Neighborhood =
    Dict (Float, Float) (List (Float, Float))


view model = 
    let 
        res = Canvas.toHtml ( cWidth, cHeight ) [ ]
            ((clear ( 0, 0 ) cWidth cHeight) ::
            (List.range 0 model.height
            |> List.map toFloat
            |> List.concatMap (\j -> 
                (List.map ((|>) j)
                (List.range 0 model.width
                    |> List.map toFloat
                    |> List.map (cell model.neighborhood))))))
    in res

update : Msg -> Model -> Model
update msg model =
    case msg of
        AddLink p1 p2 ->
            { model | linkQueue = List.append model.linkQueue [(p1, p2)]}
        Tick ->
            case model.linkQueue of
              [] -> model
              (p1,p2) :: rest -> { model | linkQueue = rest, neighborhood = addLink p1 p2 model.neighborhood }

init : () -> ( Model, Cmd Msg )
init () =
    let
        width = toFloat ((cWidth - 10) // cellWidth - 1)
        height = toFloat ((cHeight - 10) // cellWidth - 1)
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
    in
    ({ linkQueue = [], width = (round width), height = (round height), neighborhood = Dict.empty }, Cmd.batch batch)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = \model -> Time.every 50 (always Tick)
        }