module Visualisation.Visualisation exposing (..)

import Css
import Util exposing (Map, Pos)
import Visualisation.Bfs exposing (BfsVisualisation, bfsCellCss, getBfsVisualisationState, tickBfs)
import Visualisation.Dfs exposing (DfsVisualisation, dfsCellCss, getDfsVisualisationState, tickDfs)
import Visualisation.Dijkstra exposing (DijkstraVisualisation, dijkstraCellCss, getDijkstraVisualisationState, tickDijkstra)
import Visualisation.VisualisationUtil exposing (VisualisationState(..))


type Visualisation
    = NoVisualisation
    | BfsV BfsVisualisation
    | DfsV DfsVisualisation
    | DijkstraV DijkstraVisualisation


{-| Map State of each type of visualisation to VisualisationState
-}
getVisualisationState : Visualisation -> VisualisationState
getVisualisationState vis =
    case vis of
        NoVisualisation ->
            Stopped

        BfsV bfsVis ->
            getBfsVisualisationState bfsVis

        DfsV dfsVis ->
            getDfsVisualisationState dfsVis

        DijkstraV dijkstraVis ->
            getDijkstraVisualisationState dijkstraVis


{-| Do a single tick of a visualisation
-}
tickVisualisation : Map -> Visualisation -> Visualisation
tickVisualisation map visualisation =
    case visualisation of
        BfsV bfsVis ->
            BfsV (tickBfs map bfsVis)

        DfsV dfsVis ->
            DfsV (tickDfs map dfsVis)

        DijkstraV dijkstraVis ->
            DijkstraV (tickDijkstra map dijkstraVis)

        _ ->
            visualisation


{-| Get cell CSS based on visualisation
-}
visualisationCellCss : Pos -> Map -> Visualisation -> List Css.Style
visualisationCellCss pos map visualisation =
    case visualisation of
        BfsV bfsVis ->
            bfsCellCss pos map bfsVis

        DfsV dfsVis ->
            dfsCellCss pos map dfsVis

        DijkstraV dijkstraVis ->
            dijkstraCellCss pos map dijkstraVis

        _ ->
            []
