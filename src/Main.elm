module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


-- Model

type alias Model =
    { steps : Int }


-- Initial model

initialModel : Model
initialModel =
    { steps = 0 }


-- Msg

type Msg
    = Increment


-- Update

update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | steps = model.steps + 1 }


-- View

view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Steps: " ++ String.fromInt model.steps) ]
        , button [ onClick Increment ] [ text "Click me!" ]
        ]


-- Main

main =
    Browser.sandbox { init = initialModel, update = update, view = view }
