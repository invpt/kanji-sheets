module Main exposing (main)

import Browser
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view >> toUnstyled
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
        [ css
            [ displayFlex
            , flexDirection row
            , width (vw 100)
            , height (vh 100)
            , fontFamily serif
            ]
        ]
        [ div [ css [ flex (int 1) ] ] []
        , div
            [ css [ width (px 320), backgroundColor (hex "f8f8f8"), padding (px 16), overflow scroll ] ]
            [ text "Hello"
            , button [ onClick Increment ] [ text "inc" ]
            , text (String.fromInt model)
            ]
        ]
