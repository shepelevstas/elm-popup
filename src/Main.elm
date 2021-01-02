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
import Modal
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


defaultPrompt text =
    { id = "default prompt"
    , text = text
    , color = Blue
    , inputValue = ""
    , leaving = False
    }


type alias Model =
    { msg : String
    , msgCount : Int
    , msgs : List Popup
    , prompt : Maybe Prompt
    , color : Color
    , stay : Bool
    , modalState : Modal.State
    }


type alias Prompt =
    { id : String
    , text : String
    , color : Color
    , inputValue : String
    , leaving : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { msg = ""
      , msgCount = 0
      , msgs = []
      , prompt = Nothing
      , color = White
      , stay = True
      , modalState = Modal.Closed
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
    | UpdatePromptInput String
    | RemovePrompt
    | DeletePrompt
    | PopPrompt
    | PromptOk
    | CloseModal
    | ShowModal
    | ModalMsg Modal.Msg


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

        UpdatePromptInput text ->
            case model.prompt of
                Nothing ->
                    ( model, Cmd.none )

                Just prompt ->
                    ( { model | prompt = Just { prompt | inputValue = text } }, Cmd.none )

        RemovePrompt ->
            case model.prompt of
                Nothing ->
                    ( model, Cmd.none )

                Just prompt ->
                    ( { model | prompt = Just { prompt | leaving = True } }, Cmd.none )

        DeletePrompt ->
            ( { model | prompt = Nothing }, Cmd.none )

        PopPrompt ->
            if String.length model.msg > 0 then
                ( { model
                    | prompt = Just <| defaultPrompt model.msg
                    , msg = ""
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        PromptOk ->
            case model.prompt of
                Nothing ->
                    ( model, Cmd.none )

                Just prompt ->
                    ( { model
                        | msg = prompt.inputValue
                        , prompt = Just { prompt | leaving = True }
                      }
                    , Cmd.none
                    )

        CloseModal ->
            ( { model | modalState = Modal.Closed }, Cmd.none )

        ShowModal ->
            ( { model | modalState = Modal.Showing }, Cmd.none )

        ModalMsg subMsg ->
            let
                ( state, cmd ) =
                    Modal.update subMsg model.modalState
            in
            ( { model | modalState = state }, Cmd.map ModalMsg cmd )



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
            , button
                [ onClick PopPrompt, style "font-size" "24px" ]
                [ text "prompt" ]
            , button
                -- [ onClick <| ModalMsg Modal.Show ]
                [ onClick ShowModal ]
                [ text "show modal" ]
            ]
        , ul [ class "popups" ] <|
            (case model.prompt of
                Nothing ->
                    ( "no-prompt", text "" )

                Just propmt ->
                    ( "prompt", viewPopInput propmt )
            )
                :: List.map
                    (\popup -> ( popup.id, viewPopup popup ))
                    model.msgs
        , Modal.view ModalMsg
            model.modalState
            [ style "background-color" "white"
            , style "padding" "1em"
            , style "border-radius" ".5em"
            , style "box-shadow" "0 2px 6px rgba(0,0,0,.5)"
            ]
            [ text "This is Modal Content"
            , button [ onClick (ModalMsg Modal.Close) ] [ text "close" ]
            ]
        ]


viewPopup : Popup -> Html Msg
viewPopup popup =
    let
        ( color, bgcolor ) =
            colorValues popup.color
    in
    div [ class "popup" ]
        [ div
            [ class "popup__body enter"
            , classList [ ( "leave", popup.leaving ) ]
            , onAnimationsEnd [ ( "leave", DeleteMsg popup ) ]
            , style "color" color
            , style "background-color" bgcolor
            ]
            [ text popup.text

            -- CLOSE BTN
            , span
                [ class "popup__close"
                , onClick (RemovePopup popup)
                ]
                [ text "✕" ]
            ]
        ]


viewPopInput : Prompt -> Html Msg
viewPopInput prompt =
    let
        ( color, bgcolor ) =
            colorValues prompt.color
    in
    div [ class "popup" ]
        [ div
            [ class "popup__body enter"
            , style "z-index" "9999"
            , classList [ ( "leave", prompt.leaving ) ]
            , onAnimationsEnd [ ( "leave", DeletePrompt ) ]
            , style "color" color
            , style "background-color" bgcolor
            ]
            [ div [] [ text prompt.text ]
            , input
                [ style "box-sizing" "border-box"
                , style "width" "100%"
                , onInput UpdatePromptInput
                , onEnterKey PromptOk
                ]
                []
            , div
                [ style "display" "flex"
                , style "justify-content" "space-around"
                ]
                [ button [ onClick RemovePrompt ] [ text "cancel" ]
                , button [ onClick PromptOk ] [ text "ok" ]
                ]

            --CLOSE BTN
            -- , span
            --     [ class "popup__close" ]
            --     [ text "✕" ]
            ]

        -- SHADE
        , div
            [ class "fadein"
            , style "position" "fixed"
            , style "left" "0"
            , style "top" "0"
            , style "width" "100vw"
            , style "height" "100vh"
            , style "background-color" "rgba(0,0,0,.5)"
            , style "z-index" "9998"
            , onClick RemovePrompt
            , classList [ ( "fadeout", prompt.leaving ) ]
            ]
            []
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
