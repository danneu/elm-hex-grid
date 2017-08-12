module Main exposing (..)

-- Core
-- 3rd
-- 1st

import Dict
import HexGrid exposing (Direction(..), HexGrid(..))
import Html exposing (Html, a, div, h1, hr, small)
import Html.Attributes as Hattr exposing (class, href)
import Html.Events as Hevent
import Html.Lazy exposing (lazy)
import Json.Decode as JD
import Set exposing (Set)
import String
import Svg exposing (Svg, g, polygon, text, text_)
import Svg.Attributes as Sattr exposing (fill, points, stroke, x, y)
import Svg.Events as Sevent exposing (onClick, onMouseOver)


type alias Demo =
    { name : String
    , grid : HexGrid ()
    , activePoint : HexGrid.Point
    , hoverPoint : HexGrid.Point
    , obstacles : Set HexGrid.Point
    , maxSteps : Int

    -- , tilePath : Set.Set HexCoord
    -- , moveTiles : Set.Set HexCoord -- Tiles highlight user can move to
    }


initDemo : String -> Demo
initDemo name =
    { name = name
    , grid = HexGrid.empty 5 ()
    , activePoint = ( 0, 0 )
    , hoverPoint = ( -1, -4 )
    , obstacles =
        Set.fromList
            [ ( -5, 4 )
            , ( -4, 3 )
            , ( -3, 2 )
            , ( -2, 1 )
            , ( -1, 1 )
            , ( -1, 2 )
            , ( 0, 2 )
            , ( 1, 2 )
            , ( 2, 1 )
            , ( 2, 0 )
            , ( 2, -1 )
            , ( 1, -1 )
            , ( 2, -2 )
            , ( -1, -1 )
            , ( 0, -2 )
            , ( 1, -3 )
            ]
    , maxSteps = 4
    }


type alias Model =
    Dict.Dict String Demo


init : Model
init =
    List.map (\demo -> ( demo.name, demo ))
        [ initDemo "lineDemo"
        , initDemo "directionDemo"
        , initDemo "rangeDemo"
        , initDemo "obstaclesDemo"
        , initDemo "rotationDemo"
        , initDemo "ringDemo"
        , initDemo "fogDemo"
        , initDemo "pathDemo"
        , initDemo "costPathDemo"
        ]
        |> Dict.fromList


type Msg
    = NoOp
    | ActivePoint Demo HexGrid.Point
    | HoverPoint Demo HexGrid.Point
    | SetMaxSteps Demo Int
    | InsertObstacle Demo HexGrid.Point
    | RemoveObstacle Demo HexGrid.Point


forceGet : comparable -> Dict.Dict comparable v -> v
forceGet key dict =
    case Dict.get key dict of
        Just v ->
            v

        Nothing ->
            Debug.crash "Impossible"


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        ActivePoint prevDemo point ->
            let
                nextDemo =
                    { prevDemo | activePoint = point }
            in
            Dict.insert nextDemo.name nextDemo model

        HoverPoint prevDemo point ->
            let
                nextDemo =
                    { prevDemo | hoverPoint = point }
            in
            Dict.insert nextDemo.name nextDemo model

        SetMaxSteps prevDemo steps ->
            let
                nextDemo =
                    { prevDemo | maxSteps = steps }
            in
            Dict.insert nextDemo.name nextDemo model

        InsertObstacle prevDemo point ->
            let
                nextDemo =
                    { prevDemo | obstacles = Set.insert point prevDemo.obstacles }
            in
            Dict.insert nextDemo.name nextDemo model

        RemoveObstacle prevDemo point ->
            let
                nextDemo =
                    { prevDemo | obstacles = Set.remove point prevDemo.obstacles }
            in
            Dict.insert nextDemo.name nextDemo model


viewDistance : Demo -> Svg Msg
viewDistance model =
    let
        (HexGrid _ dict) =
            model.grid

        cornersToStr corners =
            corners
                |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                |> String.join " "

        -- not flipped this time
        layout =
            HexGrid.mkPointyTop 30 30 (600 / 2) (570 / 2)

        pointsInLine =
            HexGrid.line model.activePoint model.hoverPoint
                |> Set.fromList

        renderPoint ( point, tile ) =
            let
                ( centerX, centerY ) =
                    HexGrid.hexToPixel layout point

                corners =
                    HexGrid.polygonCorners layout point
            in
            g
                [ onClick (ActivePoint model point)
                , onMouseOver (HoverPoint model point)
                ]
                [ polygon
                    [ points (cornersToStr <| corners)
                    , stroke "black"
                    , fill <|
                        if model.activePoint == point then
                            "grey"
                        else if model.hoverPoint == point then
                            "#f1c40f"
                            -- gold
                        else if Set.member point pointsInLine then
                            "#bdc3c7"
                            -- light grey
                        else
                            "white"
                    ]
                    []
                ]
    in
    Svg.svg
        []
        (List.map renderPoint (Dict.toList dict))


viewDistanceDemo : Demo -> Html Msg
viewDistanceDemo demo =
    div
        [ class "row" ]
        [ div
            [ class "col-lg-6" ]
            [ Html.h2 [] [ Html.text "Line" ]
            , Html.p
                []
                [ Html.text "Get the tiles that fall on the line between two points."
                ]
            , Html.pre
                []
                [ Html.text <|
                    "HexGrid.line "
                        ++ toString demo.activePoint
                        ++ " "
                        ++ toString demo.hoverPoint
                , Html.br [] []
                , Html.text <| "=> " ++ (toString <| HexGrid.line demo.activePoint demo.hoverPoint)
                , Html.br [] []
                , Html.br [] []
                , Html.text <|
                    "HexGrid.distance "
                        ++ toString demo.activePoint
                        ++ " "
                        ++ toString demo.hoverPoint
                , Html.br [] []
                , Html.text <| "=> " ++ (toString <| HexGrid.distance demo.activePoint demo.hoverPoint)
                ]
            ]
        , div
            [ class "col-lg-6" ]
            [ viewDistance demo
            ]
        ]


viewDirectionTo : Demo -> Svg Msg
viewDirectionTo model =
    let
        (HexGrid _ dict) =
            model.grid

        cornersToStr corners =
            corners
                |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                |> String.join " "

        -- not flipped this time
        layout =
            HexGrid.mkPointyTop 30 30 (600 / 2) (570 / 2)

        direction =
            HexGrid.directionTo model.activePoint model.hoverPoint

        renderPoint ( point, tile ) =
            let
                ( centerX, centerY ) =
                    HexGrid.hexToPixel layout point

                corners =
                    HexGrid.polygonCorners layout point
            in
            g
                [ onClick (ActivePoint model point)
                , onMouseOver (HoverPoint model point)
                ]
                [ polygon
                    [ points (cornersToStr <| corners)
                    , stroke "black"
                    , fill <|
                        if model.activePoint == point then
                            "grey"
                        else if model.hoverPoint == point then
                            "#f1c40f"
                            -- gold
                        else
                            "white"
                    ]
                    []
                , text_
                    [ x (toString <| centerX - 10)
                    , y (toString <| centerY - 5)
                    , Hattr.style [ ( "font-size", "18px" ) ]
                    ]
                    [ text <|
                        if point == model.hoverPoint then
                            Maybe.withDefault "--" (Maybe.map toString direction)
                        else
                            ""
                    ]
                ]
    in
    Svg.svg
        []
        (List.map renderPoint (Dict.toList dict))


viewDirectionToDemo : Demo -> Html Msg
viewDirectionToDemo demo =
    div
        [ class "row" ]
        [ div
            [ class "col-lg-6" ]
            [ Html.h2 [] [ Html.text "Direction" ]
            , Html.p
                []
                [ Html.text "Get the numerical direction (0-5) to a destination point."
                ]
            , Html.p
                []
                [ Html.text "0 starts East and increments going counterclockwise."
                ]
            , let
                start =
                    demo.activePoint

                end =
                    demo.hoverPoint

                direction =
                    HexGrid.directionTo start end
              in
              Html.pre
                []
                [ Html.text <|
                    "HexGrid.directionTo "
                        ++ toString start
                        ++ " "
                        ++ toString end
                , Html.br [] []
                , Html.text <| "=> " ++ toString direction
                ]
            ]
        , div
            [ class "col-lg-6" ]
            [ viewDirectionTo demo
            ]
        ]


viewRange : Demo -> Svg Msg
viewRange model =
    let
        (HexGrid _ dict) =
            model.grid

        cornersToStr corners =
            corners
                |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                |> String.join " "

        -- not flipped this time
        layout =
            HexGrid.mkPointyTop 30 30 (600 / 2) (570 / 2)

        pointsInRange =
            model.activePoint
                |> HexGrid.range (HexGrid.distance model.activePoint model.hoverPoint)
                |> Set.fromList

        renderPoint ( point, tile ) =
            let
                ( centerX, centerY ) =
                    HexGrid.hexToPixel layout point

                corners =
                    HexGrid.polygonCorners layout point
            in
            g
                [ onClick (ActivePoint model point)
                , onMouseOver (HoverPoint model point)
                ]
                [ polygon
                    [ points (cornersToStr <| corners)
                    , stroke "black"
                    , fill <|
                        if model.activePoint == point then
                            "grey"
                        else if model.hoverPoint == point then
                            "#f1c40f"
                            -- gold
                        else if Set.member point pointsInRange then
                            "#bdc3c7"
                            -- light grey
                        else
                            "white"
                    ]
                    []
                ]
    in
    Svg.svg
        []
        (List.map renderPoint (Dict.toList dict))


viewRangeDemo : Demo -> Html Msg
viewRangeDemo demo =
    div
        [ class "row" ]
        [ div
            [ class "col-lg-6" ]
            [ Html.h2 [] [ Html.text "Range" ]
            , Html.p
                []
                [ Html.text "List all points that are within `radius` range of a point."
                ]
            , let
                distance =
                    HexGrid.distance demo.activePoint demo.hoverPoint
              in
              Html.pre
                []
                [ Html.text <|
                    "HexGrid.range "
                        ++ toString distance
                        ++ " "
                        ++ toString demo.activePoint
                , Html.br [] []
                , Html.text <| "=> " ++ (toString <| HexGrid.range distance demo.activePoint)
                ]
            ]
        , div
            [ class "col-lg-6" ]
            [ viewRange demo
            , viewRangeIntersection demo
            ]
        ]


viewRangeIntersection : Demo -> Svg Msg
viewRangeIntersection model =
    let
        (HexGrid _ dict) =
            model.grid

        cornersToStr corners =
            corners
                |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                |> String.join " "

        -- not flipped this time
        layout =
            HexGrid.mkPointyTop 30 30 (600 / 2) (570 / 2)

        pointsInStaticRange =
            HexGrid.range 2 ( -3, 3 )
                |> Set.fromList

        pointsInUserRange =
            HexGrid.range 2 model.hoverPoint
                |> Set.fromList

        pointsInIntersection =
            Set.intersect pointsInStaticRange pointsInUserRange

        renderPoint ( point, tile ) =
            let
                ( centerX, centerY ) =
                    HexGrid.hexToPixel layout point

                corners =
                    HexGrid.polygonCorners layout point
            in
            g
                [ onClick (ActivePoint model point)
                , onMouseOver (HoverPoint model point)
                ]
                [ polygon
                    [ points (cornersToStr <| corners)
                    , fill <|
                        if Set.member point pointsInIntersection then
                            "#9b59b6"
                        else if Set.member point pointsInStaticRange then
                            "#e74c3c"
                        else if Set.member point pointsInUserRange then
                            "#3498db"
                        else
                            "white"
                    ]
                    []
                ]
    in
    Svg.svg
        []
        (List.map renderPoint (Dict.toList dict))


viewReachable : Demo -> Svg Msg
viewReachable model =
    let
        (HexGrid _ dict) =
            model.grid

        cornersToStr corners =
            corners
                |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                |> String.join " "

        -- not flipped this time
        layout =
            HexGrid.mkPointyTop 30 30 (600 / 2) (570 / 2)

        pointsReachable =
            HexGrid.reachable ( 0, 0 ) model.maxSteps model.obstacles

        counts =
            HexGrid.stepCounts model.maxSteps model.obstacles ( 0, 0 )

        renderPoint ( point, tile ) =
            let
                ( centerX, centerY ) =
                    HexGrid.hexToPixel layout point

                corners =
                    HexGrid.polygonCorners layout point
            in
            g
                [ onClick <|
                    if Set.member point model.obstacles then
                        RemoveObstacle model point
                    else
                        InsertObstacle model point
                , onMouseOver (HoverPoint model point)
                ]
                [ polygon
                    [ points (cornersToStr <| corners)
                    , fill <|
                        if point == ( 0, 0 ) then
                            "#3498db"
                        else if model.hoverPoint == point && Set.member point model.obstacles then
                            "#c0392b"
                        else if Set.member point model.obstacles then
                            "#e74c3c"
                        else if model.hoverPoint == point then
                            "#f1c40f"
                            -- gold
                        else if Set.member point pointsReachable then
                            "white"
                        else
                            "#bdc3c7"
                    ]
                    []
                , text_
                    [ x (toString <| centerX - 10)
                    , y (toString <| centerY - 5)
                    , Hattr.style [ ( "font-size", "18px" ) ]
                    ]
                    [ text <|
                        if point == ( 0, 0 ) then
                            "👁"
                        else
                            ""
                    ]
                , text_
                    [ stroke "blue"
                    , x (toString <| centerX - 5)
                    , y (toString <| centerY + 10)
                    , Hattr.style [ ( "font-size", "18px" ) ]
                    ]
                    [ text <|
                        case Dict.get point counts of
                            Nothing ->
                                ""

                            Just count ->
                                toString count
                    ]
                ]
    in
    Svg.svg
        []
        (List.map renderPoint (Dict.toList dict))


viewReachableDemo : Demo -> Html Msg
viewReachableDemo demo =
    div
        [ class "row" ]
        [ div
            [ class "col-lg-6" ]
            [ Html.h2 [] [ Html.text "Reachability and Obstacles" ]
            , Html.p
                []
                [ Html.text "Determine the reachability of all tiles from a point, while avoiding obstacles."
                ]
            , div
                [ class "panel panel-default" ]
                [ div
                    [ class "panel-body" ]
                    [ Html.text "Max steps:"
                    , Html.input
                        [ Hattr.type_ "range"
                        , Hattr.step "1"
                        , Hattr.min "0"
                        , Hattr.max "20"
                        , Hattr.value <| toString demo.maxSteps
                        , Hevent.on "input" <|
                            JD.map (SetMaxSteps demo) (JD.at [ "target", "valueAsNumber" ] JD.int)
                        ]
                        []
                    ]
                ]
            , Html.pre
                []
                [ Html.text <|
                    "maxSteps = "
                        ++ toString demo.maxSteps
                , Html.br [] []
                , Html.br [] []
                , Html.text <|
                    "obstacles = "
                        ++ (toString <| Set.toList demo.obstacles)
                ]
            ]
        , div
            [ class "col-lg-6" ]
            [ Html.div
                [ class "alert alert-info" ]
                [ Html.text "Click tiles to create/remove obstacles"
                ]
            , viewReachable demo
            ]
        ]


viewRotation : Demo -> Svg Msg
viewRotation model =
    let
        (HexGrid _ dict) =
            model.grid

        cornersToStr corners =
            corners
                |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                |> String.join " "

        -- not flipped this time
        layout =
            HexGrid.mkPointyTop 30 30 (600 / 2) (570 / 2)

        left1 =
            HexGrid.rotate Left model.hoverPoint

        left2 =
            HexGrid.rotate Left left1

        reverse =
            HexGrid.rotate Left left2

        right1 =
            HexGrid.rotate Right model.hoverPoint

        right2 =
            HexGrid.rotate Right right1

        ( left, right ) =
            ( Set.fromList [ left1, left2 ]
            , Set.fromList [ right1, right2 ]
            )

        renderPoint ( point, tile ) =
            let
                ( centerX, centerY ) =
                    HexGrid.hexToPixel layout point

                corners =
                    HexGrid.polygonCorners layout point
            in
            g
                [ onMouseOver (HoverPoint model point)
                ]
                [ polygon
                    [ points (cornersToStr <| corners)
                    , fill <|
                        if point == ( 0, 0 ) then
                            "grey"
                        else if model.hoverPoint == point then
                            "#3498db"
                            -- blue
                        else if Set.member point left then
                            "#2ecc71"
                            -- green
                        else if Set.member point right then
                            "#9b59b6"
                            -- purple
                        else if point == reverse then
                            "#2980b9"
                            -- dark blue
                        else
                            "white"
                    ]
                    []
                , text_
                    [ stroke "white"
                    , fill "white"
                    , x (toString <| centerX - 20)
                    , y (toString <| centerY + 5)
                    , Hattr.style [ ( "font-size", "12px" ) ]
                    ]
                    [ text <|
                        if point == left1 then
                            "left1"
                        else if point == left2 then
                            "left2"
                        else if point == right1 then
                            "right1"
                        else if point == right2 then
                            "right2"
                        else if point == reverse then
                            "left3"
                        else
                            ""
                    ]
                ]
    in
    Svg.svg
        []
        (List.map renderPoint (Dict.toList dict))


viewRotationDemo : Demo -> Html Msg
viewRotationDemo demo =
    div
        [ class "row" ]
        [ div
            [ class "col-lg-6" ]
            [ Html.h2 [] [ Html.text "Rotation" ]
            , Html.p
                []
                [ Html.text "Rotate a point 60 degrees left or right."
                ]
            , let
                left1 =
                    HexGrid.rotate Left demo.hoverPoint

                left2 =
                    HexGrid.rotate Left left1

                reverse =
                    HexGrid.rotate Left left2

                right1 =
                    HexGrid.rotate Right demo.hoverPoint

                right2 =
                    HexGrid.rotate Right right1
              in
              Html.pre
                []
                [ Html.text <|
                    "HexGrid.rotate Left "
                        ++ toString demo.hoverPoint
                , Html.br [] []
                , Html.text <| "=> " ++ toString left1
                , Html.br [] []
                , Html.text <|
                    "HexGrid.rotate Left "
                        ++ toString left1
                , Html.br [] []
                , Html.text <| "=> " ++ toString left2
                , Html.br [] []
                , Html.text <|
                    "HexGrid.rotate Right "
                        ++ toString demo.hoverPoint
                , Html.br [] []
                , Html.text <| "=> " ++ toString right1
                , Html.br [] []
                , Html.text <|
                    "HexGrid.rotate Right "
                        ++ toString right1
                , Html.br [] []
                , Html.text <| "=> " ++ toString right2
                ]
            ]
        , div
            [ class "col-lg-6" ]
            [ viewRotation demo
            ]
        ]


viewSingleRing : Demo -> Svg Msg
viewSingleRing model =
    let
        (HexGrid _ dict) =
            model.grid

        cornersToStr corners =
            corners
                |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                |> String.join " "

        -- not flipped this time
        layout =
            HexGrid.mkPointyTop 30 30 (600 / 2) (570 / 2)

        pointsInRing =
            HexGrid.ring (HexGrid.distance model.hoverPoint ( 0, 0 )) ( 0, 0 )

        renderPoint ( point, tile ) =
            let
                ( centerX, centerY ) =
                    HexGrid.hexToPixel layout point

                corners =
                    HexGrid.polygonCorners layout point
            in
            g
                [ onMouseOver (HoverPoint model point)
                ]
                [ polygon
                    [ points (cornersToStr <| corners)
                    , fill <|
                        if point == ( 0, 0 ) then
                            "grey"
                        else if model.hoverPoint == point then
                            "#f1c40f"
                        else if Set.member point (Set.fromList pointsInRing) then
                            "#3498db"
                        else
                            "white"
                    ]
                    []
                ]
    in
    Svg.svg
        []
        (List.map renderPoint (Dict.toList dict))


viewSingleRingDemo : Demo -> Html Msg
viewSingleRingDemo demo =
    div
        [ class "row" ]
        [ div
            [ class "col-lg-6" ]
            [ Html.h2 [] [ Html.text "Ring" ]
            , Html.p
                []
                [ Html.text "Get all points on a ring `radius` units from a point."
                ]
            , let
                center =
                    demo.activePoint

                radius =
                    HexGrid.distance demo.hoverPoint center

                pointsInRing =
                    HexGrid.ring radius center
              in
              Html.pre
                []
                [ Html.text <|
                    "HexGrid.ring "
                        ++ toString radius
                        ++ " "
                        ++ toString center
                , Html.br [] []
                , Html.text <| "=> " ++ toString (HexGrid.ring radius center)
                ]
            ]
        , div
            [ class "col-lg-6" ]
            [ viewSingleRing demo
            ]
        ]


viewSpiralOut : Demo -> Svg Msg
viewSpiralOut model =
    let
        (HexGrid _ dict) =
            model.grid

        cornersToStr corners =
            corners
                |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                |> String.join " "

        -- not flipped this time
        layout =
            HexGrid.mkPointyTop 30 30 (600 / 2) (570 / 2)

        pointsInSpiral =
            HexGrid.spiralOut ( 0, 0 ) (HexGrid.distance model.hoverPoint ( 0, 0 ))
                |> Set.fromList

        renderPoint ( point, tile ) =
            let
                ( centerX, centerY ) =
                    HexGrid.hexToPixel layout point

                corners =
                    HexGrid.polygonCorners layout point
            in
            g
                [ onMouseOver (HoverPoint model point)
                ]
                [ polygon
                    [ points (cornersToStr <| corners)
                    , fill <|
                        if point == ( 0, 0 ) then
                            "grey"
                        else if model.hoverPoint == point then
                            "#f1c40f"
                        else if Set.member point pointsInSpiral then
                            "#3498db"
                        else
                            "white"
                    ]
                    []
                ]
    in
    Svg.svg
        []
        (List.map renderPoint (Dict.toList dict))


viewFogOfWar : Demo -> Svg Msg
viewFogOfWar model =
    let
        (HexGrid _ dict) =
            model.grid

        cornersToStr corners =
            corners
                |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                |> String.join " "

        -- not flipped this time
        layout =
            HexGrid.mkPointyTop 30 30 (600 / 2) (570 / 2)

        pointsInFog =
            HexGrid.fogOfWar ( 0, 0 ) model.obstacles model.grid

        pointsInPath =
            HexGrid.line ( 0, 0 ) model.hoverPoint
                |> List.drop 1
                |> Set.fromList

        renderPoint ( point, tile ) =
            let
                ( centerX, centerY ) =
                    HexGrid.hexToPixel layout point

                corners =
                    HexGrid.polygonCorners layout point
            in
            g
                [ onMouseOver (HoverPoint model point)
                , onClick <|
                    if Set.member point model.obstacles then
                        RemoveObstacle model point
                    else
                        InsertObstacle model point
                ]
                [ polygon
                    [ points (cornersToStr <| corners)
                    , fill <|
                        if model.hoverPoint == point && Set.member point model.obstacles then
                            "#c0392b"
                        else if Set.member point model.obstacles then
                            "#e74c3c"
                        else if point == ( 0, 0 ) then
                            "#3498db"
                        else if model.hoverPoint == point then
                            "#f1c40f"
                        else if Set.member point pointsInFog then
                            "#bdc3c7"
                        else
                            "white"
                    ]
                    []
                , text_
                    [ stroke "white"
                    , fill "white"
                    , x (toString <| centerX - 10)
                    , y (toString <| centerY + 5)
                    , Hattr.style
                        [ ( "font-family", "monospace" )
                        , ( "font-size", "18px" )
                        ]
                    ]
                    [ text <|
                        if point == ( 0, 0 ) then
                            "👁"
                        else
                            ""
                    ]
                , text_
                    [ stroke "black"
                    , fill "black"
                    , x (toString <| centerX - 8)
                    , y (toString <| centerY + 7)
                    , Hattr.style
                        [ ( "font-family", "monospace" )
                        , ( "font-size", "24px" )
                        ]
                    ]
                    [ text <|
                        if Set.member point pointsInPath then
                            "×"
                        else
                            ""
                    ]
                ]
    in
    Svg.svg
        []
        (List.map renderPoint (Dict.toList dict))


viewFogOfWarDemo : Demo -> Html Msg
viewFogOfWarDemo demo =
    div
        [ class "row" ]
        [ div
            [ class "col-lg-6" ]
            [ Html.h2 [] [ Html.text "Line of Sight (Fog of War)" ]
            , Html.p
                []
                [ Html.text "List the tiles that are obstructed from a point's view."
                ]
            , let
                eye =
                    ( 0, 0 )

                pointsInFog =
                    HexGrid.fogOfWar eye demo.obstacles demo.grid
              in
              Html.pre
                []
                [ Html.text <| "eye = " ++ toString eye
                , Html.br [] []
                , Html.br [] []
                , Html.text <| "obstacles = " ++ (toString <| Set.toList demo.obstacles)
                , Html.br [] []
                , Html.br [] []
                , Html.text <|
                    "HexGrid.fogOfWar "
                        ++ toString eye
                        ++ " obstacles grid"
                , Html.br [] []
                , Html.text <| "=> " ++ toString (Set.toList pointsInFog)
                ]
            ]
        , div
            [ class "col-lg-6" ]
            [ viewFogOfWar demo
            ]
        ]


viewPathfinding : Demo -> Svg Msg
viewPathfinding model =
    let
        (HexGrid _ dict) =
            model.grid

        cornersToStr corners =
            corners
                |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                |> String.join " "

        -- not flipped this time
        layout =
            HexGrid.mkPointyTop 30 30 (600 / 2) (570 / 2)

        graph =
            HexGrid.pathGraph ( 0, 0 ) model.hoverPoint model.obstacles model.grid

        path =
            HexGrid.pathfind ( 0, 0 ) model.hoverPoint model.obstacles model.grid

        renderPoint ( point, tile ) =
            let
                ( centerX, centerY ) =
                    HexGrid.hexToPixel layout point

                corners =
                    HexGrid.polygonCorners layout point
            in
            g
                [ onMouseOver (HoverPoint model point)
                , onClick <|
                    if Set.member point model.obstacles then
                        RemoveObstacle model point
                    else
                        InsertObstacle model point
                ]
                [ polygon
                    [ points (cornersToStr <| corners)
                    , fill <|
                        if model.hoverPoint == point && Set.member point model.obstacles then
                            "#c0392b"
                        else if Set.member point model.obstacles then
                            "#e74c3c"
                        else if point == ( 0, 0 ) then
                            "#3498db"
                        else if model.hoverPoint == point then
                            "#f1c40f"
                        else
                            "white"
                    ]
                    []
                , text_
                    [ stroke "black"
                    , fill "black"
                    , x (toString <| centerX - 10)
                    , y (toString <| centerY + 5)
                    , Hattr.style
                        [ ( "font-family", "monospace" )
                        , ( "font-size", "18px" )
                        ]
                    ]
                    [ text <|
                        if point == ( 0, 0 ) then
                            "👁"
                        else
                            ""
                    ]
                , text_
                    [ stroke "black"
                    , fill "black"
                    , x (toString <| centerX - 8)
                    , y (toString <| centerY + 7)
                    , Hattr.style
                        [ ( "font-family", "monospace" )
                        , ( "font-size", "24px" )
                        ]
                    ]
                    [ Svg.text <|
                        if List.member point path && point /= ( 0, 0 ) then
                            "×"
                        else
                            ""
                    ]
                ]
    in
    Svg.svg
        []
        (List.map renderPoint (Dict.toList dict))


viewPathfindingDemo : Demo -> Html Msg
viewPathfindingDemo demo =
    div
        [ class "row" ]
        [ div
            [ class "col-lg-6" ]
            [ Html.h2 [] [ Html.text "Breadth-First Pathfinding" ]
            , Html.p
                []
                [ Html.text "Find the shortest path to a point while navigating obstacles."
                ]
            , let
                start =
                    ( 0, 0 )

                destination =
                    demo.hoverPoint

                graph =
                    HexGrid.pathGraph start destination demo.obstacles demo.grid

                path =
                    HexGrid.pathfind start destination demo.obstacles demo.grid
              in
              Html.pre
                []
                [ Html.text <| "start = " ++ toString start
                , Html.br [] []
                , Html.text <| "destination = " ++ toString destination
                , Html.br [] []
                , Html.br [] []
                , Html.text <|
                    "HexGrid.pathfind "
                        ++ toString start
                        ++ " "
                        ++ toString destination
                        ++ " obstacles grid"
                , Html.br [] []
                , Html.text <| "=> " ++ toString path
                ]
            ]
        , div
            [ class "col-lg-6" ]
            [ viewPathfinding demo
            ]
        ]


viewPathfindingWithCost : Demo -> Svg Msg
viewPathfindingWithCost model =
    let
        (HexGrid _ dict) =
            model.grid

        cornersToStr corners =
            corners
                |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                |> String.join " "

        -- not flipped this time
        layout =
            HexGrid.mkPointyTop 30 30 (600 / 2) (570 / 2)

        --graph = HexGrid.pathGraph (0,0) model.hoverPoint model.obstacles model.grid
        calcCost point1 point2 =
            if Set.member point2 model.obstacles then
                1
            else
                6

        path =
            HexGrid.pathfindWithCost calcCost ( 0, 0 ) model.hoverPoint model.grid

        renderPoint ( point, tile ) =
            let
                ( centerX, centerY ) =
                    HexGrid.hexToPixel layout point

                corners =
                    HexGrid.polygonCorners layout point
            in
            g
                [ onMouseOver (HoverPoint model point)
                , onClick <|
                    if Set.member point model.obstacles then
                        RemoveObstacle model point
                    else
                        InsertObstacle model point
                ]
                [ polygon
                    [ points (cornersToStr <| corners)
                    , fill <|
                        if model.hoverPoint == point && Set.member point model.obstacles then
                            "#27ae60"
                        else if Set.member point model.obstacles then
                            "#2ecc71"
                        else if point == ( 0, 0 ) then
                            "#3498db"
                        else if model.hoverPoint == point then
                            "#f1c40f"
                        else
                            "white"
                    ]
                    []
                , Svg.text_
                    [ Sattr.stroke "black"
                    , Sattr.fill "black"
                    , Sattr.x (toString <| centerX - 10)
                    , Sattr.y (toString <| centerY + 5)
                    , Hattr.style
                        [ ( "font-size", "18px" )
                        ]
                    ]
                    [ Svg.text <|
                        if point == ( 0, 0 ) then
                            "👁"
                        else
                            ""
                    ]
                , Svg.text_
                    [ Sattr.stroke "black"
                    , Sattr.fill "black"
                    , Sattr.x (toString <| centerX - 8)
                    , Sattr.y (toString <| centerY + 7)
                    , Hattr.style
                        [ ( "font-size", "24px" )
                        ]
                    ]
                    [ Svg.text <|
                        if List.member point path && point /= ( 0, 0 ) then
                            "×"
                        else
                            ""
                    ]
                ]
    in
    Svg.svg
        []
        (List.map renderPoint (Dict.toList dict))


viewPathfindingWithCostDemo : Demo -> Html Msg
viewPathfindingWithCostDemo demo =
    div
        [ class "row" ]
        [ div
            [ class "col-lg-6" ]
            [ Html.h2
                []
                [ Html.text "Pathfinding with Movement Cost"
                , Html.small
                    [ Hattr.style [ ( "display", "block" ) ] ]
                    [ Html.text "Dijkstra's Algorithm" ]
                ]
            , Html.p
                []
                [ Html.text """
            You can weigh tiles with a movement cost. The pathfinder
            will prefer the cheapest route.
          """
                ]
            , Html.p
                [ class "text-muted" ]
                [ Html.text """
            In this example, roads (green tiles) have a cost of 1 while blank
            tiles have a cost of 6. You'll notice that the pathfinder will vastly
            prefer a chain of roads.
          """
                ]
            , let
                start =
                    ( 0, 0 )

                destination =
                    demo.hoverPoint

                calcCost point1 point2 =
                    if Set.member point2 demo.obstacles then
                        1
                    else
                        6

                path =
                    HexGrid.pathfindWithCost calcCost start destination demo.grid
              in
              Html.pre
                []
                [ Html.text <| "start = " ++ toString start
                , Html.br [] []
                , Html.text <| "destination = " ++ toString destination
                , Html.br [] []
                , Html.text <| "calcCost point1 point2 = "
                , Html.br [] []
                , Html.text "  if isRoad point2 then 1 else 6"
                , Html.br [] []
                , Html.br [] []
                , Html.text <|
                    "HexGrid.pathfindWithCost calcCost "
                        ++ toString start
                        ++ " "
                        ++ toString destination
                        ++ " grid"
                , Html.br [] []
                , Html.text <| "=> " ++ toString path
                ]
            ]
        , div
            [ class "col-lg-6" ]
            [ viewPathfindingWithCost demo
            ]
        ]


view : Model -> Svg Msg
view model =
    Html.div
        []
        [ lazy viewDistanceDemo (forceGet "lineDemo" model)
        , hr [] []
        , lazy viewDirectionToDemo (forceGet "directionDemo" model)
        , hr [] []
        , lazy viewRangeDemo (forceGet "rangeDemo" model)
        , hr [] []
        , lazy viewReachableDemo (forceGet "obstaclesDemo" model)
        , hr [] []
        , lazy viewRotationDemo (forceGet "rotationDemo" model)
        , hr [] []
        , lazy viewSingleRingDemo (forceGet "ringDemo" model)
        , hr [] []
        , lazy viewFogOfWarDemo (forceGet "fogDemo" model)
        , hr [] []
        , lazy viewPathfindingDemo (forceGet "pathDemo" model)
        , hr [] []
        , lazy viewPathfindingWithCostDemo (forceGet "costPathDemo" model)
        ]


main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
