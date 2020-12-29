module Main exposing (..)

import Browser
import Debug
import Events exposing (..)
import Html exposing (Html, button, div, h1, img, input, label, p, span, text)
import Html.Attributes exposing (checked, class, classList, name, src, style, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput)
import Html.Keyed exposing (ul)
import Json.Decode as JD
import List
import List.Extra as LE
import Maybe
import Process
import Set exposing (Set)
import Task



---- MODEL ----


type Color
    = White
    | Green
    | Blue


colors =
    [ White, Green, Blue ]


colorFromString val =
    case val of
        "green" ->
            Green

        "blue" ->
            Blue

        _ ->
            White


stringFromColor clr =
    case clr of
        White ->
            "white"

        Green ->
            "green"

        Blue ->
            "blue"


colorValues clr =
    case clr of
        Green ->
            ( "white", "#43B649" )

        Blue ->
            ( "white", "#51B2D6" )

        White ->
            ( "black", "white" )


type alias Popup =
    { id : String
    , text : String
    , color : Color
    , stay : Bool
    , leaving : Bool
    }


type alias Model =
    { msg : String
    , msgCount : Int
    , msgs : List Popup
    , color : Color
    , stay : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { msg = ""
      , msgCount = 0
      , msgs = []
      , color = White
      , stay = True
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateMsg String
    | PopMsg
    | RemovePopup Popup
    | DeleteMsg Popup
    | SetColor Color
    | ToggleStay


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleStay ->
            ( { model | stay = not model.stay }, Cmd.none )

        SetColor clr ->
            ( { model | color = clr }, Cmd.none )

        UpdateMsg m ->
            -- update text input state
            ( { model | msg = m }, Cmd.none )

        PopMsg ->
            -- add msg to msgs (if it has length)
            if String.length model.msg > 0 then
                let
                    popup : Popup
                    popup =
                        { id = String.fromInt model.msgCount
                        , text = model.msg
                        , color = model.color
                        , stay = model.stay
                        , leaving = False
                        }
                in
                ( { model
                    | msg = ""
                    , msgCount = model.msgCount + 1
                    , msgs = popup :: model.msgs
                  }
                , if not model.stay then
                    Process.sleep 5000 |> Task.perform (always (RemovePopup popup))

                  else
                    Cmd.none
                )

            else
                ( model, Cmd.none )

        RemovePopup popup ->
            -- the 1st step of animated remove of a popup: leave animation
            --
            -- if this msg is not in the msgs list already (was deleted manualy
            -- by the user) - don't add it to leaving set (it won't be properly
            -- removed by onAnimationsEnd event)
            ( { model
                | msgs =
                    List.map
                        (\item ->
                            if item == popup then
                                { item | leaving = True }

                            else
                                item
                        )
                        model.msgs
              }
            , Cmd.none
            )

        DeleteMsg popup ->
            -- the 2nd step of animated remove of a popup: actually delete the msg
            ( { model | msgs = List.filter ((/=) popup) model.msgs }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        radioColor clr isChecked =
            label []
                [ input
                    [ type_ "radio", name "popcolor", value (stringFromColor clr), style "vertical-align" "middle", style "margin" "0", style "padding" "0", checked isChecked, onInput (always <| SetColor clr) ]
                    []
                , span
                    [ style "display" "inline-block", style "height" "1em", style "width" "1em", style "background-color" (colorValues clr |> Tuple.second), style "vertical-align" "middle", style "border" "1px solid gray" ]
                    []
                ]
    in
    div []
        [ div [ class "bottom" ]
            [ div [ class "popup-colors" ] <|
                label []
                    [ input
                        [ type_ "checkbox"
                        , style "vertical-align" "middle"
                        , checked model.stay
                        , onInput (always ToggleStay)
                        ]
                        []
                    , text "Stay"
                    ]
                    :: List.map (\clr -> radioColor clr <| clr == model.color) colors
            , input
                [ value model.msg
                , onInput UpdateMsg
                , onEnterKey PopMsg
                , style "font-size" "24px"
                ]
                []
            , button
                [ onClick PopMsg
                , style "font-size" "24px"
                ]
                [ text "pop msg" ]
            ]
        , ul [ class "popups" ] <|
            List.map
                (\popup ->
                    ( popup.id, viewPopup popup )
                )
                model.msgs
        ]


viewPopup : Popup -> Html Msg
viewPopup popup =
    let
        ( color, bgcolor ) =
            colorValues popup.color
    in
    div [ class "popup enter" ]
        [ div
            [ class "popup__body"
            , classList [ ( "leave", popup.leaving ) ]
            , onAnimationsEnd [ ( "leave", DeleteMsg popup ) ]
            , style "color" color
            , style "background-color" bgcolor
            ]
            [ text popup.text
            , span
                [ class "popup__close"
                , onClick (RemovePopup popup)
                ]
                [ text "✕" ]
            ]
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
