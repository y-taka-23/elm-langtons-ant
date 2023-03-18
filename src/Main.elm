module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, disabled)
import Task
import Time
import List.Extra


type alias Model =
    { cells : List (List Cell)
    , count : Int
    , state : State
    , antPosition : Position
    , antDirection : Direction
    }

type State
    = Stopped
    | Running

type Cell
    = White
    | Black

type alias Position =
    { x : Int
    , y : Int
    }

type Direction
    = Up
    | Right
    | Down
    | Left


init : Model
init =
    { count = 0
    , state = Stopped
    , cells = List.repeat 100 (List.repeat 100 White)
    , antPosition = Position 50 50
    , antDirection = Up
    }


-- UPDATE

type Msg
    = Tick
    | Pause
    | Resume
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            if model.state == Running then
                let
                    (newAntPosition, newAntDirection, newCells) = moveAnt model
                in
                ( { model
                    | count = model.count + 1
                    , antPosition = newAntPosition
                    , antDirection = newAntDirection
                    , cells = newCells
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        Pause ->
            ( { model | state = Stopped }, Cmd.none )

        Resume ->
            ( { model | state = Running }, Cmd.none )

        Reset ->
            ( { model | count = 0 }, Cmd.none )


moveAnt : Model -> ( Position, Direction, List (List Cell) )
moveAnt model =
    let
        pos = model.antPosition
        currentCell = getCell pos.x pos.y model.cells
        newDirection = changeDirection model.antDirection currentCell
        newCells = flipCellColor model.antPosition model.cells
        newPosition = moveForward model.antPosition newDirection
    in
    ( newPosition, newDirection, newCells )


getCell : Int -> Int -> List (List Cell) -> Cell
getCell x y cells =
    List.Extra.getAt y cells
        |> Maybe.andThen (List.Extra.getAt x)
        |> Maybe.withDefault White


changeDirection : Direction -> Cell -> Direction
changeDirection direction cell =
    case cell of
        White ->
            turnRight direction

        Black ->
            turnLeft direction


turnRight : Direction -> Direction
turnRight direction =
    case direction of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


turnLeft : Direction -> Direction
turnLeft direction =
    case direction of
        Up ->
            Left

        Right ->
            Up

        Down ->
            Right

        Left ->
            Down


flipCellColor : Position -> List (List Cell) -> List (List Cell)
flipCellColor position cells =
    let
        x = position.x
        y = position.y
        currentCell = getCell x y cells
    in
    case currentCell of
        Black ->
            List.Extra.setAt y (List.Extra.setAt x White (Maybe.withDefault [] (List.Extra.getAt y cells))) cells

        White ->
            List.Extra.setAt y (List.Extra.setAt x Black (Maybe.withDefault [] (List.Extra.getAt y cells))) cells


moveForward : Position -> Direction -> Position
moveForward position direction =
    case direction of
        Up ->
            { position | y = position.y - 1 }

        Right ->
            { position | x = position.x + 1 }

        Down ->
            { position | y = position.y + 1 }

        Left ->
            { position | x = position.x - 1 }


-- VIEW

viewCell : Cell -> Html Msg
viewCell cell =
    let
        cellClass =
            case cell of
                White ->
                    "cell-white"

                Black ->
                    "cell-black"
    in
    div [ class ("cell " ++ cellClass) ] []

viewGrid : List (List Cell) -> Html Msg
viewGrid grid =
    div []
        (List.map (\row -> div [ class "row" ] (List.map viewCell row)) grid)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text ("Steps: " ++ String.fromInt model.count) ]
        , button [ onClick Pause, disabled (model.state /= Running) ] [ text "Pause" ]
        , button [ onClick Resume, disabled (model.state == Running) ] [ text "Resume" ]
        , button [ onClick Reset, disabled (model.state == Running) ] [ text "Reset" ]
        , viewGrid model.cells
        ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Running ->
            Time.every 100 (\_ -> Tick)

        Stopped ->
            Sub.none


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
