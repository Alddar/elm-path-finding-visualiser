module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Browser.Events
import Css exposing (border3, displayFlex, focus, height, hover, property, px, rgb, solid, width)
import Dict
import Html.Styled exposing (Attribute, Html, button, div, input, label, option, p, select, styled, text, toUnstyled)
import Html.Styled.Attributes exposing (css, draggable, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onMouseEnter, preventDefaultOn)
import Json.Decode as Decode
import List exposing (range)
import Material.Icons as Filled
import Material.Icons.Types exposing (Coloring(..))
import Set
import Svg.Styled
import Tailwind.Theme as Th
import Tailwind.Utilities as Tw
import Time
import Util exposing (Map, Pos)
import Visualisation.Bfs exposing (..)
import Visualisation.Dfs exposing (..)
import Visualisation.Dijkstra exposing (..)
import Visualisation.Visualisation exposing (..)
import Visualisation.VisualisationUtil exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view >> toUnstyled, subscriptions = subscriptions }



-- MODEL


type Speed
    = Slow
    | Fast
    | VeryFast


speedString : Speed -> String
speedString speed =
    case speed of
        Slow ->
            "Slow"

        Fast ->
            "Fast"

        VeryFast ->
            "Very Fast"


allSpeeds : List Speed
allSpeeds =
    [ Slow, Fast, VeryFast ]


speedToMs : Speed -> Float
speedToMs speed =
    case speed of
        Slow ->
            100

        Fast ->
            50

        VeryFast ->
            10


type Tool
    = None
    | Wall
    | Eraser
    | Start
    | End


type Algorithm
    = BFS
    | DFS
    | Dijkstra


allAlgs =
    [ BFS, DFS, Dijkstra ]


algString : Algorithm -> String
algString alg =
    case alg of
        BFS ->
            "BFS"

        DFS ->
            "DFS"

        Dijkstra ->
            "Dijkstra"


type alias Model =
    { map : Map
    , inputs :
        { width : String
        , height : String
        }
    , tool : Tool
    , hover : Pos
    , holdingLeftClick : Bool
    , alg : Algorithm
    , visualisation : Visualisation
    , speed : Speed
    , paused : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { map =
            { walls = Set.empty
            , width = 20
            , height = 20
            , start = ( 0, 0 )
            , end = ( 1, 1 )
            }
      , inputs =
            { width = "20"
            , height = "20"
            }
      , tool = None
      , hover = ( 0, 0 )
      , holdingLeftClick = False
      , alg = BFS
      , visualisation = NoVisualisation
      , speed = Slow
      , paused = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoAction
    | SetWidth String
    | SetHeight String
      -- selected tool
    | Tool Tool
      -- keeping the position of the last cell the user hovered on
    | Hover Pos
      -- clicked or released button
    | Mouse Bool
      -- clear all walls
    | Clear
      -- Simulation controls
    | Algorithm Algorithm
    | Speed Speed
      -- Visual controls
    | StartVisual
    | PauseVisual
    | StopVisual
      -- Two different Ticks, one for manual (debug) and one from Timer
    | VisualTick
    | VisualTickTime Time.Posix


updateModel : Msg -> Model -> Model
updateModel msg ({ inputs, map } as model) =
    case msg of
        NoAction ->
            model

        SetWidth val ->
            case String.toInt val of
                Nothing ->
                    { model | inputs = { inputs | width = val } }

                Just intVal ->
                    { model | map = { map | width = intVal }, inputs = { inputs | width = val } }

        SetHeight val ->
            case String.toInt val of
                Nothing ->
                    { model | inputs = { inputs | height = val } }

                Just intVal ->
                    { model | map = { map | height = intVal }, inputs = { inputs | height = val } }

        Tool tool ->
            if tool == model.tool then
                { model | tool = None }

            else
                { model | tool = tool }

        Hover coords ->
            let
                newModel =
                    { model | hover = coords }
            in
            case ( model.tool, model.holdingLeftClick ) of
                ( Wall, True ) ->
                    { newModel | map = { map | walls = Set.insert coords model.map.walls } }

                ( Eraser, True ) ->
                    { newModel | map = { map | walls = Set.remove coords model.map.walls } }

                _ ->
                    newModel

        Mouse click ->
            let
                newModel =
                    { model | holdingLeftClick = click }
            in
            case ( model.tool, click ) of
                ( Wall, True ) ->
                    { newModel | map = { map | walls = Set.insert model.hover model.map.walls } }

                ( Eraser, True ) ->
                    { newModel | map = { map | walls = Set.remove model.hover model.map.walls } }

                ( Start, True ) ->
                    if model.map.end == model.hover then
                        newModel

                    else
                        { newModel | map = { map | start = model.hover } }

                ( End, True ) ->
                    if model.map.start == model.hover then
                        newModel

                    else
                        { newModel | map = { map | end = model.hover } }

                _ ->
                    newModel

        Clear ->
            { model | map = { map | walls = Set.empty } }

        Algorithm alg ->
            { model | alg = alg }

        Speed speed ->
            { model | speed = speed }

        PauseVisual ->
            { model | paused = not model.paused }

        StopVisual ->
            { model | paused = False, visualisation = NoVisualisation }

        StartVisual ->
            case model.alg of
                BFS ->
                    { model | visualisation = BfsV (newBfsVisualisation model.map.start), paused = False }

                DFS ->
                    { model | visualisation = DfsV (newDfsVisualisation model.map.start), paused = False }

                Dijkstra ->
                    { model | visualisation = DijkstraV (newDijkstraVisualisation model.map), paused = False }

        VisualTick ->
            { model | visualisation = tickVisualisation model.map model.visualisation }

        VisualTickTime _ ->
            { model | visualisation = tickVisualisation model.map model.visualisation }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )



-- SUBSCRIPTIONS


{-| Subscribing to onMouseUp to stop drawing when the user releases mouse button. Also subscribing to Time if the visualisation should be ticked
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onMouseUp (Decode.succeed (Mouse False))
        , case model.visualisation of
            NoVisualisation ->
                Sub.none

            _ ->
                if not model.paused && getVisualisationState model.visualisation == Running then
                    Time.every (speedToMs model.speed) VisualTickTime

                else
                    Sub.none
        ]



-- VIEW


canStartVisual : Model -> Bool
canStartVisual model =
    let
        state =
            getVisualisationState model.visualisation
    in
    case state of
        Stopped ->
            True

        Running ->
            model.paused


canPauseVisual : Model -> Bool
canPauseVisual model =
    let
        state =
            getVisualisationState model.visualisation
    in
    case state of
        Stopped ->
            False

        Running ->
            not model.paused


icon : Material.Icons.Types.Icon Msg -> Int -> Svg.Styled.Svg Msg
icon i size =
    Svg.Styled.fromUnstyled (i size Inherit)


{-| Default cell CSS
-}
styledCell : List (Attribute msg) -> List (Html msg) -> Html msg
styledCell =
    styled div
        [ width (px 30)
        , height (px 30)
        , border3 (px 1) solid (rgb 0 0 0)
        , property "user-drag" "none"
        , Tw.border
        , Tw.border_color Th.stone_900
        , Tw.flex
        , Tw.items_center
        , Tw.justify_center
        ]


{-| Cell component, also gets styles from visualisation
-}
cell : Model -> Pos -> Html Msg
cell model pos =
    let
        baseCell =
            \x y -> styledCell (x ++ [ onMouseEnter (Hover pos), draggable "false" ]) y
    in
    if model.map.start == pos then
        baseCell [ css [ Tw.text_color Th.green_600 ] ] [ icon Filled.start 30 ]

    else if model.map.end == pos then
        baseCell [ css [ Tw.text_color Th.red_700 ] ] [ icon Filled.flag 30 ]

    else if Set.member pos model.map.walls then
        baseCell [ css [ Tw.bg_color Th.zinc_300, Tw.text_color Th.red_800 ] ] [ icon Filled.window 50 ]

    else
        baseCell [ css (visualisationCellCss pos model.map model.visualisation) ] []


{-| Row Component
-}
row : List (Attribute msg) -> List (Html msg) -> Html msg
row =
    styled div
        [ displayFlex
        ]


{-| Grid component
-}
grid : Model -> Html Msg
grid model =
    div [ preventDefaultOn "mousedown" (Decode.succeed ( Mouse True, True )), css [ Tw.mt_4 ] ]
        (List.map
            (\y ->
                row [ draggable "false" ]
                    (List.map (\x -> cell model ( x, y )) (List.range 0 (model.map.width - 1)))
            )
            (List.range 0 (model.map.height - 1))
        )


{-| Select component
-}
selectWithOptions : List a -> a -> (a -> String) -> (a -> Msg) -> Html Msg
selectWithOptions options selected optionToString setOption =
    select
        [ value (optionToString selected)
        , onInput
            (\val ->
                List.foldl
                    (\opt action ->
                        if optionToString opt == val then
                            setOption opt

                        else
                            action
                    )
                    NoAction
                    options
            )
        , css
            [ Tw.rounded_lg
            , Tw.p_2
            , Tw.cursor_pointer
            , Tw.border_color Th.gray_200
            , Tw.border
            , Tw.border_solid
            , focus [ Tw.outline_none, Tw.border_color Th.gray_600 ]
            ]
        ]
        (List.map (\opt -> option [ value (optionToString opt) ] [ text (optionToString opt) ]) options)


{-| Styles of a button
-}
myButton : Bool -> Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
myButton selected disabled =
    styled button
        ([ Tw.cursor_pointer
         , Tw.rounded_lg
         , Tw.border
         , Tw.border_color Th.gray_200
         , Tw.bg_color Th.transparent
         , Tw.flex
         , Tw.items_center
         , Tw.justify_center
         , Tw.w_10
         , Tw.h_10
         , Tw.text_color Th.gray_200
         , Tw.border_solid
         , hover
            (if disabled then
                [ Tw.cursor_not_allowed ]

             else
                [ Tw.bg_color Th.gray_600 ]
            )
         ]
            ++ (if disabled then
                    [ Tw.bg_color Th.gray_400, Tw.border_color Th.gray_400, Tw.text_color Th.gray_300 ]

                else if selected then
                    [ Tw.bg_color Th.gray_600 ]

                else
                    []
               )
        )


{-| Styles of an input
-}
myInput : List (Attribute Msg) -> List (Html Msg) -> Html Msg
myInput =
    styled input
        [ focus [ Tw.outline_none, Tw.border_color Th.gray_600 ]
        , Tw.p_2
        , Tw.border_solid
        , Tw.border
        , Tw.border_color Th.gray_200
        , Tw.rounded_lg
        ]


{-| Default CSS for labels
-}
labelCss : Attribute msg
labelCss =
    css
        [ Tw.text_color Th.gray_100
        , Tw.p_1
        ]


visualisationControls : Model -> Html Msg
visualisationControls model =
    div [ css [ Tw.flex, Tw.flex_col, Tw.w_2over5, Tw.justify_center, Tw.h_full, Tw.ml_6 ] ]
        [ div [ css [ Tw.grid, Tw.grid_cols_2, Tw.gap_x_3 ] ]
            [ label [ labelCss ] [ text "Algorithm: " ]
            , label [ labelCss ] [ text "Speed: " ]
            , selectWithOptions allAlgs model.alg algString Algorithm
            , selectWithOptions allSpeeds model.speed speedString Speed
            ]
        , div [ css [ Tw.flex, Tw.items_center, Tw.justify_center, Tw.gap_3, Tw.mt_4 ] ]
            [ myButton False (not (canStartVisual model)) [ onClick StartVisual ] [ icon Filled.play_arrow 25 ]
            , myButton False (not (canPauseVisual model)) [ onClick PauseVisual ] [ icon Filled.pause 25 ]
            , myButton False False [ onClick StopVisual ] [ icon Filled.stop 25 ]
            ]
        ]


{-| Contains the dimension controls
-}
dimensionControls : Model -> Html Msg
dimensionControls model =
    div
        [ css [ Tw.w_2over5, Tw.flex, Tw.flex_col ] ]
        [ label [ labelCss ] [ text "Width: " ]
        , myInput [ type_ "text", value model.inputs.width, onInput SetWidth ] []
        , label [ labelCss ] [ text "Height: " ]
        , myInput [ type_ "text", value model.inputs.height, onInput SetHeight ] []
        ]


{-| Contains all the buttons with tools that can be used
-}
toolBar : Model -> Html Msg
toolBar model =
    div [ css [ Tw.w_full, Tw.flex, Tw.gap_3, Tw.justify_center, Tw.p_3 ] ]
        [ myButton (model.tool == Wall) False [ onClick (Tool Wall) ] [ icon Filled.window 20 ]
        , myButton (model.tool == Eraser) False [ onClick (Tool Eraser) ] [ icon Filled.check_box_outline_blank 20 ]
        , myButton (model.tool == Start) False [ onClick (Tool Start) ] [ icon Filled.start 20 ]
        , myButton (model.tool == End) False [ onClick (Tool End) ] [ icon Filled.flag 20 ]
        , myButton False False [ onClick Clear ] [ icon Filled.clear 20 ]
        ]


{-| Contains all the controls of the simulation
-}
controls : Model -> Html Msg
controls model =
    div [ css [ Tw.w_full, Tw.bg_color Th.gray_500 ] ]
        [ div [ css [ Tw.w_full, Tw.flex, Tw.justify_center, Tw.items_center ] ] [ dimensionControls model, visualisationControls model ]
        , toolBar model
        ]


view : Model -> Html Msg
view model =
    div [ css [ Tw.flex, Tw.flex_col, Tw.content_center, Tw.items_center ] ]
        [ controls model
        , grid model
        ]
