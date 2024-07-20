module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Html.Attributes exposing (style)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    Int


init : Model
init =
    0


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : Model -> Html Msg
view model =
    div
        [ class "top" ]
        [ div [ style "flex" "1" ] []
        , div
            [ class "sidebar" ]
            [ text "Hello"
            , button [ onClick Increment ] [ text "inc" ]
            , text (String.fromInt model)
            ]
        ]
