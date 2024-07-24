module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src, style)
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
        (field "o" (list string))
        (field "k" (list string))


type Msg
    = GotBubun (Result Http.Error Bubun)
    | SelectedKanji Kanji
    | DeselectedKanji Kanji


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBubun (Ok bubun) ->
            ( { model | jlptData = JlptData bubun }, Cmd.none )

        GotBubun (Err _) ->
            ( { model | jlptData = JlptDataFailure "Couldn't load the JLPT data :(" }, Cmd.none )

        SelectedKanji kanji ->
            ( { model | selectedKanji = model.selectedKanji ++ [ kanji ] }
            , Cmd.none
            )

        DeselectedKanji kanji ->
            ( { model | selectedKanji = List.filter ((/=) kanji) model.selectedKanji }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ class "top" ]
        [ div
            [ class "sidebar" ]
            [ jlptDataView model.jlptData model.selectedKanji
            ]
        , div [ style "flex" "1" ] []
        , div [ class "output" ] (List.map kanjiInfoView model.selectedKanji)
        , div [ style "flex" "1" ] []
        ]


kanjiInfoView : Kanji -> Html Msg
kanjiInfoView kanji =
    div [ class "kanji-practice-item" ]
        [ dl
            [ class "kanji-description" ]
            [ dt [] [ text "Meaning" ]
            , dd [] [ text (String.join ", " kanji.imi) ]
            , dt [] [ text "On'yomi" ]
            , dd [] [ text (String.join ", " kanji.onyomi) ]
            , dt [] [ text "Kun'yomi" ]
            , dd [] [ text (String.join ", " kanji.kunyomi) ]
            ]
        , div
            [ class "kanji-visuals" ]
            [ div [ class "kanji-stroke-order" ] [ text kanji.ji ]
            , div [ class "kanji-practice-boxes" ]
                (List.map
                    (\i -> kanjiPracticeBoxView kanji (i <= 6))
                    (List.range 1 24)
                )
            ]
        ]


kanjiPracticeBoxView : Kanji -> Bool -> Html Msg
kanjiPracticeBoxView kanji showHint =
    if showHint then
        div [ class "kanji-practice-box kanji-practice-hint" ]
            [ img [ class "kanji-practice-guide-lines", src "images/guide-lines.svg" ] []
            , text kanji.ji
            ]

    else
        div [ class "kanji-practice-box" ]
            [ img [ class "kanji-practice-guide-lines", src "images/guide-lines.svg" ] [] ]


jlptDataView : JlptData -> List Kanji -> Html Msg
jlptDataView data selectedKanji =
    case data of
        JlptData bubun ->
            bubunView bubun selectedKanji

        JlptDataFailure message ->
            div [] [ text message ]

        JlptDataLoading ->
            div [] [ text "Loading..." ]


bubunView : Bubun -> List Kanji -> Html Msg
bubunView bubun selectedKanji =
    div [ class "bubun" ]
        [ div [ class "bubun-meishou" ] [ text bubun.meishou ]
        , bubunNaiyouView bubun.naiyou selectedKanji
        ]


bubunNaiyouView : BubunNaiyou -> List Kanji -> Html Msg
bubunNaiyouView bubunNaiyou selectedKanji =
    case bubunNaiyou of
        KanjiBubunNaiyou kanji ->
            div [ class "kanji-bubun-naiyou" ]
                (List.map
                    (\k -> kanjiButtonView k (List.any ((==) k) selectedKanji))
                    kanji
                )

        BubunBubunNaiyou innerBubun ->
            div [ class "bubun-bubun-naiyou" ] (List.map (\b -> bubunView b selectedKanji) innerBubun)


kanjiButtonView : Kanji -> Bool -> Html Msg
kanjiButtonView kanji isSelected =
    let
        attrs =
            if isSelected then
                [ class "kanji-button selected", onClick (DeselectedKanji kanji) ]

            else
                [ class "kanji-button", onClick (SelectedKanji kanji) ]
    in
    button attrs [ text kanji.ji ]
