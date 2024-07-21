module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, dict, field, list, map3, string)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Model =
    { jlptData : JlptData
    , selectedKanji : KanjiCategory
    }


type JlptData
    = JlptData KanjiCategories
    | JlptDataFailure String
    | JlptDataLoading


init : () -> ( Model, Cmd Msg )
init _ =
    ( { jlptData = JlptDataLoading, selectedKanji = Dict.empty }, getJlptData )


getJlptData : Cmd Msg
getJlptData =
    Http.get
        { url = "data/jlpt.json"
        , expect = Http.expectJson GotJlptData kanjiCategoriesDecoder
        }


type alias KanjiCategories =
    Dict String KanjiCategory


type alias KanjiCategory =
    Dict String KanjiInfo


type alias KanjiInfo =
    { meanings : List String
    , onyomi : List String
    , kunyomi : List String
    }


kanjiCategoriesDecoder : Decoder KanjiCategories
kanjiCategoriesDecoder =
    dict kanjiCategoryDecoder


kanjiCategoryDecoder : Decoder KanjiCategory
kanjiCategoryDecoder =
    dict kanjiInfoDecoder


kanjiInfoDecoder : Decoder KanjiInfo
kanjiInfoDecoder =
    map3 KanjiInfo
        (field "m" (list string))
        (field "k" (list string))
        (field "o" (list string))


type Msg
    = GotJlptData (Result Http.Error KanjiCategories)
    | SelectedKanji String KanjiInfo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotJlptData (Ok cats) ->
            ( { model | jlptData = JlptData cats }, Cmd.none )

        GotJlptData (Err _) ->
            ( { model | jlptData = JlptDataFailure "Couldn't load the JLPT data :(" }, Cmd.none )

        SelectedKanji kanji info ->
            ( { model
                | selectedKanji = Dict.update kanji (\_ -> Just info) model.selectedKanji
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ class "top" ]
        [ div [ style "flex" "1" ] (List.map kanjiInfoView (Dict.toList model.selectedKanji))
        , div
            [ class "sidebar-wrapper" ]
            [ div
                [ class "sidebar" ]
                [ text "Hello"
                , jlptDataView model.jlptData
                ]
            ]
        ]


kanjiInfoView : ( String, KanjiInfo ) -> Html Msg
kanjiInfoView ( kanji, info ) =
    div [] [ text kanji ]


jlptDataView : JlptData -> Html Msg
jlptDataView data =
    case data of
        JlptData cats ->
            div [ class "kanji-categories" ] (List.map kanjiSelectorCategoryView (Dict.toList cats))

        JlptDataFailure _ ->
            div [] []

        JlptDataLoading ->
            div [] []


kanjiSelectorCategoryView : ( String, KanjiCategory ) -> Html Msg
kanjiSelectorCategoryView ( name, cat ) =
    div [ class "kanji-category" ] (List.map kanjiButtonView (Dict.toList cat))


kanjiButtonView : ( String, KanjiInfo ) -> Html Msg
kanjiButtonView ( kanji, info ) =
    button [ class "kanji-button", onClick (SelectedKanji kanji info) ] [ text kanji ]
