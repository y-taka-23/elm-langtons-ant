module Main exposing (..)

import Browser
import Html exposing (Html, div, button, text, h1)
import Html.Attributes exposing (class, disabled)
import Time exposing (every)
import Html.Events exposing (onClick)


-- MODEL

type Cell
    = White
    | Black

type alias Model =
    { count : Int
    , state : State
    , cells : List (List Cell)
    }

type State
    = Running
    | Paused


initCells : Int -> Int -> List (List Cell)
initCells rows cols =
    List.repeat rows (List.repeat cols White)

init : Model
init =
    { count = 0
    , state = Paused
    , cells = initCells 100 100
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
                ( { model | count = model.count + 1 }, Cmd.none )
            else
                ( model, Cmd.none )

        Pause ->
            ( { model | state = Paused }, Cmd.none )

        Resume ->
            ( { model | state = Running }, Cmd.none )

        Reset ->
            ( { model | count = 0 }, Cmd.none )


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
    if model.state == Running then
        Time.every 100 (\_ -> Tick)
    else
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
