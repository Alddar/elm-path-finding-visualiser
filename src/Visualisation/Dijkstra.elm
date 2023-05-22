module Visualisation.Dijkstra exposing (..)

import Css
import Dict exposing (Dict)
import Set exposing (Set)
import Tailwind.Theme as Th
import Tailwind.Utilities as Tw
import Util exposing (Map, Pos, nonVisitedNeighbors)
import Visualisation.VisualisationUtil exposing (VisualisationState(..))
import List exposing (range)


type DijkstraStep
    = DijkstraFind
    | DijkstraBacktrack
    | DijkstraDone
    | DijkstraError


type alias DijkstraVisualisation =
    { unvisited : List Pos
    , visited : Set Pos
    , dist : Dict Pos Int
    , parentDict : Dict Pos Pos
    , step : DijkstraStep
    , path : List Pos
    }


dijkstraStepString : DijkstraStep -> String
dijkstraStepString step =
    case step of
        DijkstraFind ->
            "FIND"

        DijkstraBacktrack ->
            "BACKTRACK"

        DijkstraDone ->
            "DONE"

        DijkstraError ->
            "ERROR"


maybeDistCompare : Maybe Int -> Maybe Int -> Order
maybeDistCompare dist1 dist2 =
    case ( dist1, dist2 ) of
        ( Nothing, Nothing ) ->
            EQ

        ( Nothing, _ ) ->
            GT

        ( _, Nothing ) ->
            LT

        ( Just d1, Just d2 ) ->
            compare d1 d2


dijkstraCompare : Dict Pos Int -> Pos -> Pos -> Order
dijkstraCompare dict pos1 pos2 =
    let
        dist1 =
            Dict.get pos1 dict

        dist2 =
            Dict.get pos2 dict
    in
    maybeDistCompare dist1 dist2


{-| Single tick of Dijkstra when doing backtrack
-}
tickDijkstraBacktrack : Map -> DijkstraVisualisation -> DijkstraVisualisation
tickDijkstraBacktrack { start } vis =
    let
        currentPathNode =
            List.head vis.path

        nextPathNode =
            Maybe.andThen (\node -> Dict.get node vis.parentDict) currentPathNode
    in
    case nextPathNode of
        Nothing ->
            { vis | step = DijkstraError }

        Just pathNode ->
            if start == pathNode then
                { vis | step = DijkstraDone, path = pathNode :: vis.path }

            else
                { vis | path = pathNode :: vis.path }


{-| Single tick of Dijkstra when doing find
-}
tickDijkstraFind : Map -> DijkstraVisualisation -> DijkstraVisualisation
tickDijkstraFind ({ end } as map) vis =
    let
        current =
            List.head vis.unvisited

        rest =
            List.drop 1 vis.unvisited
    in
    case current of
        Nothing ->
            { vis | step = DijkstraError }

        Just pos ->
            if end == pos then
                { vis | step = DijkstraBacktrack, path = [ pos ] }

            else
                let
                    neighbors =
                        nonVisitedNeighbors map vis.visited pos

                    currentPosDistMaybe =
                        Dict.get pos vis.dist
                in
                case currentPosDistMaybe of
                    Nothing ->
                        { vis | step = DijkstraError }

                    Just currentPosDist ->
                        let
                            parentDict =
                                List.foldl
                                    (\neighbor dict ->
                                        if maybeDistCompare (Dict.get neighbor vis.dist) (Just (currentPosDist + 1)) == GT then
                                            Dict.insert neighbor pos dict

                                        else
                                            dict
                                    )
                                    vis.parentDict
                                    neighbors

                            dist =
                                List.foldl
                                    (\neighbor dict ->
                                        if maybeDistCompare (Dict.get neighbor vis.dist) (Just (currentPosDist + 1)) == GT then
                                            Dict.insert neighbor (currentPosDist + 1) dict

                                        else
                                            dict
                                    )
                                    vis.dist
                                    neighbors
                        in
                        { vis
                            | parentDict = parentDict
                            , dist = dist
                            , visited = Set.insert pos vis.visited
                            , unvisited = List.sortWith (dijkstraCompare dist) rest
                        }


tickDijkstra : Map -> DijkstraVisualisation -> DijkstraVisualisation
tickDijkstra map vis =
    case vis.step of
        DijkstraFind ->
            tickDijkstraFind map vis

        DijkstraBacktrack ->
            tickDijkstraBacktrack map vis

        _ ->
            vis


getDijkstraVisualisationState : DijkstraVisualisation -> VisualisationState
getDijkstraVisualisationState vis =
    case vis.step of
        DijkstraFind ->
            Running

        DijkstraBacktrack ->
            Running

        DijkstraDone ->
            Stopped

        DijkstraError ->
            Stopped


dijkstraCellCss : Pos -> Map -> DijkstraVisualisation -> List Css.Style
dijkstraCellCss pos _ vis =
    if List.member pos vis.path then
        [ Tw.bg_color Th.orange_400 ]

    else if Set.member pos vis.visited then
        [ Tw.bg_color Th.emerald_500 ]

    else
        []


newDijkstraVisualisation : Map -> DijkstraVisualisation
newDijkstraVisualisation map = 
                    let
                        allNodes =
                            List.concatMap (\x -> List.map (\y -> ( x, y )) (range 0 (map.height - 1))) (range 0 (map.width - 1))

                        distDict =
                            Dict.insert map.start 0 Dict.empty

                        allNodesSorted =
                            List.sortWith (dijkstraCompare distDict) allNodes
                    in
                    { unvisited = allNodesSorted, visited = Set.empty, parentDict = Dict.empty, dist = distDict, step = DijkstraFind, path = [] }