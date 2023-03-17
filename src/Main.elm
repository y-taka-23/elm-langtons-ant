module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (disabled)
import Time exposing (every)
import Html.Events exposing (onClick)


-- MODEL

type alias Model =
    { count : Int
    , state : State
    }

type State
    = Running
    | Paused

init : Model
init =
    { count = 0
    , state = Running
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

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text ("Steps: " ++ String.fromInt model.count) ]
        , button [ onClick Pause, disabled (model.state /= Running) ] [ text "Pause" ]
        , button [ onClick Resume, disabled (model.state == Running) ] [ text "Resume" ]
        , button [ onClick Reset, disabled (model.state == Running) ] [ text "Reset" ]
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
