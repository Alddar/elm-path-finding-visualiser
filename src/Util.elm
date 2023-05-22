module Util exposing (..)

import Set exposing (Set)


type alias Pos =
    ( Int, Int )


type alias Map =
    { walls : Set Pos
    , width : Int
    , height : Int
    , start : Pos
    , end : Pos
    }


{-| Is the Pos inside the grid specified in Map
-}
isInGrid : Map -> Pos -> Bool
isInGrid { width, height } ( x, y ) =
    x >= 0 && x < width && y >= 0 && y < height


{-| Relative positions of all the neighbors
-}
neighbourDifference : List Pos
neighbourDifference =
    [ ( -1, 0 ), ( 0, -1 ), ( 0, 1 ), ( 1, 0 ) ]


{-| Absolute positions of all the neighbors
-}
neighbours : Pos -> List Pos
neighbours ( x, y ) =
    List.map (\( dx, dy ) -> ( x + dx, y + dy )) neighbourDifference


{-| All neighbors, excluding the ones outside the grid and walls
-}
allNeighbors : Map -> Pos -> List Pos
allNeighbors map coords =
    List.filter (\pos -> isInGrid map pos && not (Set.member pos map.walls)) (neighbours coords)


{-| All neighbors, but also check if not in supplied set
-}
nonVisitedNeighbors : Map -> Set Pos -> Pos -> List Pos
nonVisitedNeighbors map visited coords =
    List.filter (\pos -> not (Set.member pos visited)) (allNeighbors map coords)
