module Modal exposing (..)

import Events exposing (onAnimationsEnd)
import Html exposing (div, text)
import Html.Attributes exposing (class, classList, style)
import Html.Events.Extra exposing (onClickStopPropagation)
import Process
import Task


type Msg
    = NoOp
    | Remove
    | Show
    | Close


type State
    = Showing
    | Leaving
    | Closed


update msg state =
    case msg of
        NoOp ->
            ( state, Cmd.none )

        Close ->
            ( Leaving, Process.sleep 2000 |> Task.perform (always Remove) )

        Remove ->
            ( Closed, Cmd.none )

        Show ->
            ( Showing, Cmd.none )



-- view : List (Html.Attribute) -> List (Html)


view toMsg state attrs content =
    -- if state == Closed then
    --     text ""
    -- else
    let
        displayed =
            style "display"
                (if state == Closed then
                    "none"

                 else
                    "block"
                )

        opacity =
            style "opacity"
                (if state == Showing then
                    "1"

                 else
                    "0"
                )

        sliding =
            style "transform"
                (if state == Showing then
                    "translateY(20px)"

                 else
                    "translateY(0px)"
                )
    in
    div
        [ style "position" "fixed"
        , style "top" "0"
        , style "left" "0"
        , style "right" "0"
        , style "bottom" "0"
        , displayed
        ]
        [ div
            [ style "width" "100%"
            , style "height" "100%"
            , style "background-color" "rgba(0,0,0,.5)"
            , style "transition" "opacity 2s"
            , opacity

            -- , class "fadein"
            -- , classList
            --     [ ( "fadeout", state == Leaving )
            --     , ( "fadein", state == Showing )
            --     ]
            ]
            []
        , div
            [ style "position" "fixed"
            , style "top" "0"
            , style "left" "0"
            , style "right" "0"
            , style "bottom" "0"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "transition" "opacity 2s, transform 2s"
            , opacity
            , sliding

            -- , class "enter-fade-slideup"
            -- , classList
            --     [ ( "leave-fade-slidedown", state == Leaving )
            --     , ( "enter-fade-slideup", state == Showing )
            --     ]
            ]
            [ div (onClickStopPropagation (toMsg NoOp) :: attrs) content ]
        ]
