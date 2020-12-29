module Events exposing (..)

import Html
import Html.Events exposing (on)
import Json.Decode as JD
import List.Extra as LE


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


onEnterKey : msg -> Html.Attribute msg
onEnterKey msg =
    on "keydown"
        (JD.andThen
            (\code ->
                if code == 13 then
                    JD.succeed msg

                else
                    JD.fail "Not Enter Key"
            )
            Html.Events.keyCode
        )


onEnterKeyInputValue : (String -> msg) -> Html.Attribute msg
onEnterKeyInputValue tagger =
    let
        inputValueOnEnterKey : JD.Decoder String
        inputValueOnEnterKey =
            JD.field "key" JD.string
                |> JD.andThen
                    (\key ->
                        if key == "Enter" then
                            JD.at [ "target", "value" ] JD.string

                        else
                            JD.fail "not ENTER"
                    )
    in
    on "keydown" <|
        JD.map tagger inputValueOnEnterKey
