module Visualisation.Bfs exposing (..)

{-| This module contains functions for the BFS algorithm
-}

import Css
import Dict exposing (Dict)
import Set exposing (Set)
import Tailwind.Theme as Th
import Tailwind.Utilities as Tw
import Util exposing (Map, Pos, nonVisitedNeighbors)
import Visualisation.VisualisationUtil exposing (VisualisationState(..))


type BfsStep
    = BfsFind
    | BfsBacktrack
    | BfsDone
    | BfsError


bfsStepString : BfsStep -> String
bfsStepString step =
    case step of
        BfsFind ->
            "FIND"

        BfsBacktrack ->
            "BACKTRACK"

        BfsDone ->
            "DONE"

        BfsError ->
            "ERROR"


type alias BfsVisualisation =
    { toVisit : List Pos -- FIFO
    , visited : Set Pos
    , parentDict : Dict Pos Pos
    , path : List Pos
    , step : BfsStep
    }


{-| Single tick of BFS when doing backtrack
-}
tickBfsBacktrack : Map -> BfsVisualisation -> BfsVisualisation
tickBfsBacktrack { start } vis =
    let
        currentPathNode =
            List.head vis.path

        nextPathNode =
            Maybe.andThen (\node -> Dict.get node vis.parentDict) currentPathNode
    in
    case nextPathNode of
        Nothing ->
            { vis | step = BfsError }

        Just pathNode ->
            if start == pathNode then
                { vis | step = BfsDone, path = pathNode :: vis.path }

            else
                { vis | path = pathNode :: vis.path }


{-| Single tick of BFS when doing find
-}
tickBfsFind : Map -> BfsVisualisation -> BfsVisualisation
tickBfsFind ({ end } as map) vis =
    let
        current =
            List.head vis.toVisit

        rest =
            List.drop 1 vis.toVisit
    in
    case current of
        Nothing ->
            { vis | step = BfsError }

        Just pos ->
            if end == pos then
                { vis | path = [ pos ], step = BfsBacktrack }

            else if Set.member pos vis.visited then
                tickBfsFind map { vis | toVisit = rest, step = BfsFind }

            else
                let
                    addToVisit =
                        nonVisitedNeighbors map vis.visited pos
                in
                { vis
                    | toVisit = rest ++ addToVisit
                    , visited = Set.insert pos vis.visited
                    , parentDict =
                        List.foldl
                            (\visit dict ->
                                if not (Dict.member visit dict) then
                                    Dict.insert visit pos dict

                                else
                                    dict
                            )
                            vis.parentDict
                            addToVisit
                }


tickBfs : Map -> BfsVisualisation -> BfsVisualisation
tickBfs map vis =
    case vis.step of
        BfsFind ->
            tickBfsFind map vis

        BfsBacktrack ->
            tickBfsBacktrack map vis

        _ ->
            vis


getBfsVisualisationState : BfsVisualisation -> VisualisationState
getBfsVisualisationState vis =
    case vis.step of
        BfsFind ->
            Running

        BfsBacktrack ->
            Running

        BfsDone ->
            Stopped

        BfsError ->
            Stopped


bfsCellCss : Pos -> Map -> BfsVisualisation -> List Css.Style
bfsCellCss pos _ vis =
    if List.member pos vis.path then
        [ Tw.bg_color Th.orange_400 ]

    else if Set.member pos vis.visited then
        [ Tw.bg_color Th.emerald_500 ]

    else if List.member pos vis.toVisit then
        [ Tw.bg_color Th.teal_300 ]

    else
        []


newBfsVisualisation : Pos -> BfsVisualisation
newBfsVisualisation start =
    { toVisit = [ start ], visited = Set.empty, parentDict = Dict.empty, path = [], step = BfsFind }
