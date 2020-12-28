module Main exposing (..)

import Browser
import Debug
import Html exposing (Html, button, div, h1, img, input, p, span, text)
import Html.Attributes exposing (class, classList, src, style, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as JD
import List
import List.Extra as LE
import Maybe
import Set exposing (Set)



---- MODEL ----


type alias Model =
    { msg : String
    , msgs : List String
    , info : String
    , fadingout : Set String
    , entering : Set String
    }


init : ( Model, Cmd Msg )
init =
    ( { msg = ""
      , msgs = []
      , info = ""
      , fadingout = Set.empty
      , entering = Set.empty
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | PopMsg
    | InputEnter String
    | MsgText String
    | Delete String
    | Fadeout String
    | Slidein String
    | RemoveFromEntering String


addMsg model msg =
    if String.length msg == 0 then
        model

    else
        { model
            | msgs = msg :: model.msgs
            , msg = ""
            , entering = Set.insert msg model.entering
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PopMsg ->
            ( addMsg model model.msg, Cmd.none )

        InputEnter value ->
            ( addMsg model value, Cmd.none )

        Delete m ->
            ( { model | msgs = List.filter ((/=) m) model.msgs, fadingout = Set.remove m model.fadingout }, Cmd.none )

        MsgText m ->
            ( { model | msg = m }, Cmd.none )

        Slidein m ->
            ( { model | entering = Set.insert m model.entering }, Cmd.none )

        Fadeout m ->
            ( { model | fadingout = Set.insert m model.fadingout }, Cmd.none )

        RemoveFromEntering m ->
            ( { model | entering = Set.remove m model.entering }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [ class "bottom" ]
            [ input
                [ value model.msg
                , onInput MsgText
                , onInputEnterKey InputEnter
                , style "font-size" "24px"
                ]
                []
            , button
                [ onClick PopMsg
                , style "font-size" "24px"
                ]
                [ text "pop msg" ]
            ]
        , div
            [ class "popups" ]
            (div [] [ text model.info ]
                :: List.map
                    (\m ->
                        viewPopup
                            m
                            (Set.member m model.entering)
                            (Set.member m model.fadingout)
                    )
                    model.msgs
            )
        ]


viewPopup : String -> Bool -> Bool -> Html Msg
viewPopup m isEntering isFading =
    div
        [ class "popup"
        , onAnimationsEnd
            [ ( "fadeout", Delete m )
            , ( "popup", RemoveFromEntering m )
            ]
        , classList
            [ ( "fadeout", isFading )
            , ( "slidein", isEntering )
            ]
        ]
        [ text m
        , span
            [ class "close-popup"
            , onClick (Fadeout m)
            ]
            [ text "✕" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



---- EVENTS ----


onAnimationsEnd : List ( String, msg ) -> Html.Attribute msg
onAnimationsEnd map =
    let
        animationNameDecoder : JD.Decoder String
        animationNameDecoder =
            JD.field "animationName" JD.string
    in
    on "animationend" <|
        JD.andThen
            (\name ->
                case LE.find (Tuple.first >> (==) name) map of
                    Just ( _, msg ) ->
                        JD.succeed msg

                    Nothing ->
                        JD.fail "animationName not mapped to any msg"
            )
            animationNameDecoder


onInputEnterKey : (String -> msg) -> Html.Attribute msg
onInputEnterKey tagger =
    let
        inputValueOnEnterKey : JD.Decoder String
        inputValueOnEnterKey =
            JD.field "code" JD.string
                |> JD.andThen
                    (\code ->
                        if code == "Enter" then
                            JD.at [ "target", "value" ] JD.string

                        else
                            JD.fail "not ENTER"
                    )
    in
    on "keydown" <|
        JD.map tagger inputValueOnEnterKey
