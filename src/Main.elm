module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, lazy, list, map2, map4, oneOf, string)


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
    , selectedKanji : List Kanji
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { jlptData = JlptDataLoading, selectedKanji = [] }, getJlptData )


getJlptData : Cmd Msg
getJlptData =
    Http.get
        { url = "data/jlpt.json"
        , expect = Http.expectJson GotBubun bubunDecoder
        }


type JlptData
    = JlptDataLoading
    | JlptDataFailure String
    | JlptData Bubun


type alias Bubun =
    { meishou : String
    , naiyou : BubunNaiyou
    }


type BubunNaiyou
    = KanjiBubunNaiyou (List Kanji)
    | BubunBubunNaiyou (List Bubun)


type alias Kanji =
    { ji : String
    , imi : List String
    , onyomi : List String
    , kunyomi : List String
    }


bubunDecoder : Decoder Bubun
bubunDecoder =
    map2 Bubun
        (field "m" string)
        (oneOf [ field "k" kanjiBubunNaiyouDecoder, field "b" bubunBubunNaiyouDecoder ])


kanjiBubunNaiyouDecoder : Decoder BubunNaiyou
kanjiBubunNaiyouDecoder =
    Json.Decode.map KanjiBubunNaiyou (list kanjiDecoder)


bubunBubunNaiyouDecoder : Decoder BubunNaiyou
bubunBubunNaiyouDecoder =
    Json.Decode.map BubunBubunNaiyou (list (lazy (\_ -> bubunDecoder)))


kanjiDecoder : Decoder Kanji
kanjiDecoder =
    map4 Kanji
        (field "j" string)
        (field "i" (list string))
        (field "k" (list string))
        (field "o" (list string))


type Msg
    = GotBubun (Result Http.Error Bubun)
    | SelectedKanji Kanji


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBubun (Ok bubun) ->
            ( { model | jlptData = JlptData bubun }, Cmd.none )

        GotBubun (Err _) ->
            ( { model | jlptData = JlptDataFailure "Couldn't load the JLPT data :(" }, Cmd.none )

        SelectedKanji kanji ->
            ( { model
                | selectedKanji = kanji :: model.selectedKanji
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ class "top" ]
        [ div [ style "flex" "1" ] (List.map kanjiInfoView model.selectedKanji)
        , div
            [ class "sidebar-wrapper" ]
            [ div
                [ class "sidebar" ]
                [ text "Hello"
                , jlptDataView model.jlptData
                ]
            ]
        ]


kanjiInfoView : Kanji -> Html Msg
kanjiInfoView kanji =
    div [] [ text kanji.ji ]


jlptDataView : JlptData -> Html Msg
jlptDataView data =
    case data of
        JlptData bubun ->
            div [ class "kanji-categories" ] [ kanjiSelectorCategoryView bubun ]

        JlptDataFailure _ ->
            div [] []

        JlptDataLoading ->
            div [] []


kanjiSelectorCategoryView : Bubun -> Html Msg
kanjiSelectorCategoryView bubun =
    case bubun.naiyou of
        KanjiBubunNaiyou kanji ->
            div [ class "kanji-category" ] (List.map kanjiButtonView kanji)

        BubunBubunNaiyou innerBubun ->
            div [ class "kanji-categories" ] (List.map kanjiSelectorCategoryView innerBubun)


kanjiButtonView : Kanji -> Html Msg
kanjiButtonView kanji =
    button [ class "kanji-button", onClick (SelectedKanji kanji) ] [ text kanji.ji ]
