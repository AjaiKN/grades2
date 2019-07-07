module Helpers exposing (divClass, divClass1, floatToStr, numInput, strToFloat)

{-| Helper functions needed in other modules.
-}

import Html exposing (Html, div, input, label, span, text)
import Html.Attributes exposing (class, step, type_, value)
import Html.Events exposing (onInput)
import Round


numInput : Bool -> (String -> msg) -> String -> String -> Html msg
numInput isPercent changer val lab =
    Html.span []
        [ label [] [ text lab ]
        , divClass "input-group"
            [ input
                [ class "numberInput form-control"
                , onInput changer
                , type_ "number"
                , value val
                , step "any"
                ]
                []
            , if isPercent then
                divClass "input-group-append" [ span [ class "input-group-text" ] [ text "%" ] ]

              else
                div [] []
            ]
        ]


divClass c =
    div [ class c ]


divClass1 c thing =
    div [ class c ] [ thing ]


strToFloat : String -> Float
strToFloat =
    String.toFloat >> Maybe.withDefault 0.0


floatToStr : Float -> String
floatToStr =
    Round.roundNum 3 >> String.fromFloat
