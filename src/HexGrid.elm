module HexGrid exposing (..)

-- Core
-- 1st

import Array exposing (Array)
import Dict exposing (Dict)
import PairingHeap exposing (PairingHeap)
import Set exposing (Set)


type alias Point =
    ( Int, Int )



-- Int is the grid radius.


type HexGrid a
    = HexGrid Int (Dict Point a)


toPoint : Int -> Int -> Point
toPoint x z =
    ( x, z )



-- UTIL


axialToCube : Point -> ( Int, Int, Int )
axialToCube ( x, z ) =
    ( x, -x - z, z )



-- CREATE


empty : Int -> a -> HexGrid a
empty levels a =
    let
        row y =
            List.range 0 (levels * 2 - abs (levels - y))
                |> List.map (\n -> ( ( n - min y levels, y - levels ), a ))
    in
        HexGrid levels (Dict.fromList <| List.concatMap (\n -> row n) (List.range 0 (2 * levels)))



-- Create a grid from a list of (Point, val) tuples


fromList : Int -> a -> List ( Point, a ) -> HexGrid a
fromList radius defaultVal pairs =
    let
        reducer ( coord, val ) memo =
            insert coord val memo
    in
        List.foldl reducer (empty radius defaultVal) pairs



-- TRANSFORM


{-| Map a function over each (Pont, val) pair, updating the
value in each cell.
-}
map : (Point -> a -> b) -> HexGrid a -> HexGrid b
map xform (HexGrid a dict) =
    dict
        |> Dict.map xform
        |> HexGrid a


foldl : (Point -> a -> b -> b) -> b -> HexGrid a -> b
foldl step init (HexGrid a dict) =
    Dict.foldl step init dict


{-| Update value at coord
-}
update : Point -> (a -> a) -> HexGrid a -> HexGrid a
update point xform ((HexGrid r dict) as grid) =
    case valueAt point grid of
        Nothing ->
            grid

        Just val ->
            insert point (xform val) grid


insert : Point -> a -> HexGrid a -> HexGrid a
insert point v ((HexGrid r dict) as grid) =
    if not (contains point grid) then
        grid
    else
        HexGrid r (Dict.insert point v dict)


remove : Point -> HexGrid a -> HexGrid a
remove point (HexGrid r dict) =
    HexGrid r (Dict.remove point dict)



-- QUERY


pathfind : Point -> Point -> Set Point -> HexGrid a -> List Point
pathfind start end obstacles grid =
    let
        graph =
            pathGraph start end obstacles grid

        accum point path =
            case Dict.get point graph of
                Nothing ->
                    --path
                    [ start ]

                Just maybeNext ->
                    case maybeNext of
                        Nothing ->
                            List.append path [ end ]

                        Just next ->
                            accum next (next :: path)
    in
        if Set.member start obstacles then
            []
        else
            accum end []



{- Returns dictionary of Point -> PrevPoint. To get from start to end,
   you follow the mapping til you arrive at start, and it will have generated
   a node path.

   Passing in the `end` target point lets us short-circuit.
-}


pathGraph : Point -> Point -> Set Point -> HexGrid a -> Dict Point (Maybe Point)
pathGraph start end obstacles grid =
    let
        accum : List Point -> Dict Point (Maybe Point) -> Dict Point (Maybe Point)
        accum frontier cameFrom =
            --let _ = Debug.log "frontier" frontier in
            case frontier of
                [] ->
                    cameFrom

                curr :: rest ->
                    -- Short-circuit if we encounter our target point.
                    -- No point in building the graph further.
                    if curr == end then
                        cameFrom
                    else
                        let
                            ( frontier_, cameFrom_ ) =
                                List.foldl
                                    (pathGraphHelp grid curr)
                                    ( rest, cameFrom )
                                    (neighbors curr
                                        |> List.filter (\p -> not <| Set.member p obstacles)
                                    )
                        in
                            accum frontier_ cameFrom_
    in
        accum [ start ] (Dict.singleton start Nothing)



-- Run for each neighbor that is not a wall


pathGraphHelp :
    HexGrid a
    -> Point
    -> Point
    -> ( List Point, Dict Point (Maybe Point) )
    -> ( List Point, Dict Point (Maybe Point) )
pathGraphHelp grid curr next ( frontier, cameFrom ) =
    if Dict.member next cameFrom then
        ( frontier, cameFrom )
    else if not <| contains next grid then
        ( frontier, cameFrom )
    else
        ( List.append frontier [ next ]
        , Dict.insert next (Just curr) cameFrom
        )



-- Run for each neighbor that is not a wall


fringesHelp : Point -> ( Set Point, Set Point ) -> ( Set Point, Set Point )
fringesHelp nbor ( visited, fringeLevel ) =
    if Set.member nbor visited then
        -- If nbor is already visited, do nothing
        ( visited, fringeLevel )
    else
        -- Else, since nbor is not a wall, add it to the fringe level
        -- and mark it as visited
        ( Set.insert nbor visited
        , Set.insert nbor fringeLevel
        )


fringes : Point -> Int -> Set Point -> Array (Set Point)
fringes start maxSteps obstacles =
    let
        accum : Int -> Set Point -> Array (Set Point) -> Array (Set Point)
        accum currStep visited fringe =
            if currStep > maxSteps then
                fringe
            else
                let
                    prevStepPoints : List Point
                    prevStepPoints =
                        case Array.get (currStep - 1) fringe of
                            Just points ->
                                Set.toList points

                            Nothing ->
                                Debug.crash "Impossible"

                    ( visited_, fringeLevel ) =
                        List.foldl
                            fringesHelp
                            ( visited, Set.empty )
                            (List.concatMap neighbors prevStepPoints
                                |> List.filter (\p -> not <| Set.member p obstacles)
                            )
                in
                    accum (currStep + 1) visited_ (Array.push fringeLevel fringe)
    in
        -- Short-circuit if starting point is an obstacle
        if Set.member start obstacles then
            Array.empty
        else
            accum 1 (Set.singleton start) (Array.fromList [ Set.singleton start ])


{-| Returns set of points reachable from a starting point within
`maxSteps`. Pathfinds around obstacles.
-}
reachable : Point -> Int -> Set Point -> Set Point
reachable start maxSteps obstacles =
    fringes start maxSteps obstacles
        |> Array.toList
        |> List.concatMap Set.toList
        |> Set.fromList


{-| Count how many steps it would take to reach a point considering
any obstacles. If Nothing, then `end` point is unreachable.

FIXME: This is slow to do per cell.
using distance is wrong since it is straight line...

-}
countSteps : Point -> Point -> Set Point -> Int -> Maybe Int
countSteps start end obstacles maxSteps =
    let
        fringe =
            fringes start maxSteps obstacles

        accum : ( Int, Set Point ) -> Maybe Int -> Maybe Int
        accum ( level, pts ) memo =
            --let _ = Debug.log "(level, pts)" (level, pts)_ =
            case memo of
                -- Bail if we already found the end point
                Just _ ->
                    memo

                -- Else, check if end point is at this depth
                Nothing ->
                    ---let _ = Debug.log "pts" pts in
                    if Set.member end pts then
                        Just level
                    else
                        Nothing
    in
        Array.foldl accum Nothing (Array.indexedMap (,) fringe)


{-| Creates a map of Point -> stepCount to reach that point.

This replaces countSteps since it is generated once instead of for
each cell.

-}
stepCounts : Int -> Set Point -> Point -> Dict Point Int
stepCounts maxSteps obstacles start =
    let
        fringe =
            fringes start maxSteps obstacles

        accum ( level, pts ) dict =
            Set.foldl (\p -> Dict.insert p level) dict pts
    in
        Array.foldl accum Dict.empty (Array.indexedMap (,) fringe)


{-| Returns set of points that point `eye` cannot see
because its view is obstructed by obstacles.
-}
fogOfWar : Point -> Set Point -> HexGrid a -> Set Point
fogOfWar eye obstacles grid =
    let
        accum : Point -> a -> Set Point -> Set Point
        accum end _ obstructed =
            let
                path =
                    line eye end

                fogAccum point ( hitWall, fogs ) =
                    if hitWall then
                        ( True, Set.insert point fogs )
                    else if Set.member point obstacles then
                        ( True, Set.insert point fogs )
                    else
                        ( False, fogs )

                ( _, fogs ) =
                    List.foldl fogAccum ( False, Set.empty ) path
            in
                Set.union obstructed fogs
    in
        foldl accum Set.empty grid


{-| Return list of (Point, val) that satisfy the predicate.
-}
filter : (( Point, a ) -> Bool) -> HexGrid a -> List ( Point, a )
filter pred (HexGrid _ dict) =
    Dict.toList dict
        |> List.filter pred


type Direction
    = Left
    | Right


rotate : Direction -> Point -> Point
rotate direction point =
    let
        ( x, y, z ) =
            axialToCube point
    in
        case direction of
            Right ->
                ( -z, -y )

            Left ->
                ( -y, -x )


{-| Tests whether given point is within grid.
-}
contains : Point -> HexGrid a -> Bool
contains (( x, z ) as coord) (HexGrid radius _) =
    let
        offset =
            radius * 2 - abs z
    in
        if z < -radius then
            False
        else if z > radius then
            False
        else if x + radius + min 0 z < 0 then
            False
        else if x > offset - radius - min 0 z then
            False
        else
            True


{-| Get the cells in the grid's outer layer.
-}
outermost : HexGrid a -> List ( Point, a )
outermost ((HexGrid r _) as grid) =
    let
        pred ( point, _ ) =
            let
                ( x, y, z ) =
                    axialToCube point
            in
                x
                    == r
                    || x
                    == -r
                    || y
                    == r
                    || y
                    == -r
                    || z
                    == r
                    || z
                    == -r
    in
        filter pred grid


{-| Get the numerical direction from one point
to another.
-}
directionTo : Point -> Point -> Maybe Int
directionTo p1 p2 =
    case line p1 p2 of
        _ :: neighbor :: _ ->
            let
                ( dx, dz ) =
                    pointSubtract neighbor p1
            in
                case ( dx, dz ) of
                    ( 1, 0 ) ->
                        Just 0

                    ( 1, -1 ) ->
                        Just 1

                    ( 0, -1 ) ->
                        Just 2

                    ( -1, 0 ) ->
                        Just 3

                    ( -1, 1 ) ->
                        Just 4

                    ( 0, 1 ) ->
                        Just 5

                    _ ->
                        Nothing

        _ ->
            Nothing


{-| Get the cell at the given point.
-}
valueAt : Point -> HexGrid a -> Maybe a
valueAt point (HexGrid _ dict) =
    Dict.get point dict


neighbors : Point -> List Point
neighbors ( x, z ) =
    [ toPoint (x + 1) z
    , toPoint (x + 1) (z - 1)
    , toPoint x (z - 1)
    , toPoint (x - 1) z
    , toPoint (x - 1) (z + 1)
    , toPoint x (z + 1)
    ]


diagonals : Point -> List Point
diagonals ( x, z ) =
    [ toPoint (x + 2) (z - 1)
    , toPoint (x + 1) (z - 2)
    , toPoint (x - 1) (z - 1)
    , toPoint (x - 2) (z + 1)
    , toPoint (x - 1) (z + 2)
    , toPoint (x + 1) (z + 1)
    ]


distance : Point -> Point -> Int
distance coord1 coord2 =
    let
        ( x1, y1, z1 ) =
            axialToCube coord1

        ( x2, y2, z2 ) =
            axialToCube coord2
    in
        ((abs <| x1 - x2) + (abs <| y1 - y2) + (abs <| z1 - z2)) // 2


{-| Check if two grids have equal contents.
-}
equal : HexGrid a -> HexGrid a -> Bool
equal (HexGrid r1 dict1) (HexGrid r2 dict2) =
    if r1 /= r2 then
        False
    else if Dict.toList dict1 /= Dict.toList dict2 then
        False
    else
        True


height : HexGrid a -> Int
height (HexGrid r _) =
    2 * r + 1


{-| Return list of Points that form shortest straight line between to Points.
-}
line : Point -> Point -> List Point
line p1 p2 =
    if p1 == p2 then
        []
    else
        let
            n =
                distance p1 p2

            pts =
                List.map toFloat (List.range 0 n)
        in
            List.map
                (\i ->
                    pointMult p1 (1 - (i / toFloat n))
                        |> pointAdd (pointMult p2 (i / toFloat n))
                        |> pointRound
                )
                pts


{-| Gets list of all points within `n` radius of given point.
-}
range : Int -> Point -> List Point
range n ( x, z ) =
    List.concatMap
        (\dx ->
            List.map
                (\dz -> toPoint (x + dx) (z + dz))
                (List.range (max -n (-dx - n)) (min n (-dx + n)))
        )
        (List.range -n n)


{-| Returns list of points that form a ring of radius `r` around
a Point.
-}
ring : Int -> Point -> List Point
ring r ( x, z ) =
    let
        p =
            toPoint (x - r) (z + r)

        -- move southwest r tiles
    in
        if r < 0 then
            []
        else
            List.scanl
                (\i p_ -> neighbor i p_)
                p
                (List.concatMap (\j -> List.repeat r j) (List.range 0 5))


{-| Start at the center and spiral outwards until all points at
radius `r` are included. Spiral jumps out to the southwest tile.
-}
spiralOut : Point -> Int -> List Point
spiralOut (( x, z ) as center) r =
    let
        accum currRadius points =
            if currRadius > r then
                points
            else
                let
                    pointsInRing =
                        ring currRadius center
                in
                    accum (currRadius + 1) (List.append points pointsInRing)
    in
        accum 1 [ center ]


{-| 0 is the adjacent cell to the east. Increments
counterclockwise.
-}
neighbor : Int -> Point -> Point
neighbor direction ( x, z ) =
    let
        ( dx, dz ) =
            case direction of
                0 ->
                    ( 1, 0 )

                1 ->
                    ( 1, -1 )

                2 ->
                    ( 0, -1 )

                3 ->
                    ( -1, 0 )

                4 ->
                    ( -1, 1 )

                5 ->
                    ( 0, 1 )

                _ ->
                    ( 0, 0 )
    in
        toPoint (x + dx) (z + dz)



-- POINT MATH


pointRound : ( Float, Float ) -> Point
pointRound ( x, z ) =
    let
        y =
            -x - z

        ( rx, ry, rz ) =
            ( round x, round y, round z )

        dx =
            abs (toFloat rx - x)

        dy =
            abs (toFloat ry - y)

        dz =
            abs (toFloat rz - z)
    in
        if dx > dy && dx > dz then
            ( -ry - rz, rz )
        else if dy > dz then
            ( rx, rz )
        else
            ( rx, -rx - ry )


pointMult : Point -> Float -> ( Float, Float )
pointMult ( x, z ) n =
    ( toFloat x * n, toFloat z * n )


pointAdd : ( number, number ) -> ( number, number ) -> ( number, number )
pointAdd ( x1, z1 ) ( x2, z2 ) =
    ( x1 + x2, z1 + z2 )


pointSubtract : ( number, number ) -> ( number, number ) -> ( number, number )
pointSubtract ( x1, z1 ) ( x2, z2 ) =
    ( x1 - x2, z1 - z2 )



-- RENDER


type Orientation
    = PointyTop
    | FlatTop


type alias Layout =
    { orientation : Orientation
    , screenX : Float
    , screenY : Float
    , originX : Float
    , originY : Float
    }


getOrientationParams : Orientation -> ( Float, Float, Float, Float, Float, Float, Float, Float, Float )
getOrientationParams orientation =
    case orientation of
        PointyTop ->
            ( sqrt 3.0
            , sqrt 3.0 / 2.0
            , 0.0
            , 3.0 / 2.0
            , sqrt 3.0 / 3.0
            , -1.0 / 3.0
            , 0.0
            , 2.0 / 3.0
            , 0.5
            )

        FlatTop ->
            ( 3.0 / 2.0
            , 0.0
            , sqrt 3.0 / 2.0
            , sqrt 3.0
            , 2.0 / 3.0
            , 0.0
            , -1.0 / 3.0
            , sqrt 3.0 / 3.0
            , 0.0
            )


mkPointyTop : Float -> Float -> Float -> Float -> Layout
mkPointyTop screenX screenY originX originY =
    Layout PointyTop screenX screenY originX originY


mkFlatTop : Float -> Float -> Float -> Float -> Layout
mkFlatTop screenX screenY originX originY =
    Layout FlatTop screenX screenY originX originY


hexToPixel : Layout -> Point -> ( Float, Float )
hexToPixel layout ( q, r ) =
    let
        ( f0, f1, f2, f3, _, _, _, _, _ ) =
            getOrientationParams layout.orientation

        x =
            (f0 * toFloat q + f1 * toFloat r) * layout.screenX

        y =
            (f2 * toFloat q + f3 * toFloat r) * layout.screenY
    in
        ( x + layout.originX, y + layout.originY )


hexCornerOffset : Layout -> Float -> ( Float, Float )
hexCornerOffset layout corner =
    let
        ( _, _, _, _, _, _, _, _, startAngle ) =
            getOrientationParams layout.orientation

        angle =
            2.0 * pi * (corner + startAngle) / 6
    in
        ( layout.screenX * cos angle, layout.screenY * sin angle )


polygonCorners : Layout -> Point -> List ( Float, Float )
polygonCorners layout point =
    let
        ( centerX, centerY ) =
            hexToPixel layout point

        accum i corners =
            if i == 6 then
                corners
            else
                let
                    ( offsetX, offsetY ) =
                        hexCornerOffset layout i

                    newCorners =
                        List.append
                            corners
                            [ ( centerX + offsetX, centerY + offsetY ) ]
                in
                    accum (i + 1) newCorners
    in
        accum 0 []



-- MOVEMENT COSTS / DIJKSTRA'S ALGORITHM
-- Obstacles and roads are now dictated with calcCost function


pathfindWithCost : (Point -> Point -> Int) -> Point -> Point -> HexGrid a -> List Point
pathfindWithCost calcCost start end grid =
    let
        graph =
            pathGraph2 calcCost start end grid

        accum point path =
            case Dict.get point graph of
                Nothing ->
                    --path
                    [ start ]

                Just maybeNext ->
                    case maybeNext of
                        Nothing ->
                            List.append path [ end ]

                        Just next ->
                            accum next (next :: path)
    in
        accum end []


pathGraph2 : (Point -> Point -> Int) -> Point -> Point -> HexGrid a -> Dict Point (Maybe Point)
pathGraph2 calcCost start end grid =
    let
        accum :
            PairingHeap Int Point
            -> Dict Point Int
            -> Dict Point (Maybe Point)
            -> Dict Point (Maybe Point)
        accum frontier costSoFar cameFrom =
            case PairingHeap.toSortedList frontier of
                [] ->
                    cameFrom

                ( priority, curr ) :: rest ->
                    -- Short-circuit if we encounter our target point.
                    -- No point in building the graph further.
                    if curr == end then
                        cameFrom
                    else
                        let
                            ( frontier_, costSoFar_, cameFrom_ ) =
                                List.foldl
                                    (pathGraphHelp2 calcCost grid curr)
                                    ( PairingHeap.fromList rest, costSoFar, cameFrom )
                                    (neighbors curr)
                        in
                            accum frontier_ costSoFar_ cameFrom_
    in
        accum
            (PairingHeap.singleton 0 start)
            (Dict.singleton start 0)
            (Dict.singleton start Nothing)



-- Run for each neighbor that is not a wall


pathGraphHelp2 :
    (Point -> Point -> Int)
    -> HexGrid a
    -> Point
    -> Point
    -> ( PairingHeap Int Point, Dict Point Int, Dict Point (Maybe Point) )
    -> ( PairingHeap Int Point, Dict Point Int, Dict Point (Maybe Point) )
pathGraphHelp2 calcCost grid curr next (( frontier, costSoFar, cameFrom ) as memo) =
    --let _ = Debug.log "[pathGraphHelp2] curr, next" (curr, next) in
    if not <| contains next grid then
        ( frontier, costSoFar, cameFrom )
    else
        let
            currCost =
                case Dict.get curr costSoFar of
                    Just cost ->
                        cost

                    Nothing ->
                        Debug.crash "Impossible"

            newCost =
                currCost + calcCost curr next
        in
            if (not <| Dict.member next costSoFar) || newCost < currCost then
                -- if newCost >= currCost then
                --   (frontier, costSoFar, cameFrom)
                ( -- frontier
                  PairingHeap.insert ( newCost, next ) frontier
                  -- costSoFar
                , Dict.insert next newCost costSoFar
                  -- cameFrom
                , Dict.insert next (Just curr) cameFrom
                )
            else
                ( frontier, costSoFar, cameFrom )
