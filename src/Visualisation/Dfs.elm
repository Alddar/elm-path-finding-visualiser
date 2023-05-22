module Visualisation.Dfs exposing (..)

import Css
import Dict exposing (Dict)
import Set exposing (Set)
import Tailwind.Theme as Th
import Tailwind.Utilities as Tw
import Util exposing (Map, Pos, allNeighbors)
import Visualisation.VisualisationUtil exposing (VisualisationState(..))


type DfsStep
    = DfsFind
    | DfsBacktrack
    | DfsDone
    | DfsError


dfsStepString : DfsStep -> String
dfsStepString step =
    case step of
        DfsFind ->
            "FIND"

        DfsBacktrack ->
            "BACKTRACK"

        DfsDone ->
            "DONE"

        DfsError ->
            "ERROR"


type alias DfsVisualisation =
    { toVisit : List Pos -- LIFO
    , visited : Set Pos
    , parentDict : Dict Pos Pos
    , path : List Pos
    , step : DfsStep
    }


{-| Single tick of DFS when doing backtrack
-}
tickDfsBacktrack : Map -> DfsVisualisation -> DfsVisualisation
tickDfsBacktrack { start } vis =
    let
        currentPathNode =
            List.head vis.path

        nextPathNode =
            Maybe.andThen (\node -> Dict.get node vis.parentDict) currentPathNode
    in
    case nextPathNode of
        Nothing ->
            { vis | step = DfsError }

        Just pathNode ->
            if start == pathNode then
                { vis | step = DfsDone, path = pathNode :: vis.path }

            else
                { vis | path = pathNode :: vis.path }


{-| Single tick of DFS when doing find
-}
tickDfsFind : Map -> DfsVisualisation -> DfsVisualisation
tickDfsFind ({ end } as map) vis =
    let
        current =
            List.head vis.toVisit

        rest =
            List.drop 1 vis.toVisit
    in
    case current of
        Nothing ->
            { vis | step = DfsError }

        Just pos ->
            if end == pos then
                { vis | path = [ pos ], step = DfsBacktrack }

            else if Set.member pos vis.visited then
                tickDfs map { vis | toVisit = rest, step = DfsFind }

            else
                let
                    addToVisit =
                        allNeighbors map pos
                in
                { vis
                    | toVisit = addToVisit ++ rest
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


tickDfs : Map -> DfsVisualisation -> DfsVisualisation
tickDfs map vis =
    case vis.step of
        DfsFind ->
            tickDfsFind map vis

        DfsBacktrack ->
            tickDfsBacktrack map vis

        _ ->
            vis


getDfsVisualisationState : DfsVisualisation -> VisualisationState
getDfsVisualisationState vis =
    case vis.step of
        DfsFind ->
            Running

        DfsBacktrack ->
            Running

        DfsDone ->
            Stopped

        DfsError ->
            Stopped


dfsCellCss : Pos -> Map -> DfsVisualisation -> List Css.Style
dfsCellCss pos _ vis =
    if List.member pos vis.path then
        [ Tw.bg_color Th.orange_400 ]

    else if Set.member pos vis.visited then
        [ Tw.bg_color Th.emerald_500 ]

    else if List.member pos vis.toVisit then
        [ Tw.bg_color Th.teal_300 ]

    else
        []


newDfsVisualisation : Pos -> DfsVisualisation
newDfsVisualisation start =
    { toVisit = [ start ], visited = Set.empty, parentDict = Dict.empty, path = [], step = DfsFind }
