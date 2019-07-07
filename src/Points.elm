module Points exposing (Model, Msg, map, toPercents, update, view)

{-| Model, update, and view for the input of the Points tab.
-}

import Helpers exposing (divClass, divClass1, floatToStr, numInput, strToFloat)
import Html exposing (Html, input, label, span, text)
import Html.Attributes exposing (attribute, class, step, type_, value)
import Html.Events exposing (onInput)



--MODEL


type alias Model t =
    { pointsNumerator : t
    , pointsDenominator : t
    , asstPoints : t
    }


map : (a -> b) -> Model a -> Model b
map f { pointsNumerator, pointsDenominator, asstPoints } =
    { pointsNumerator = f pointsNumerator
    , pointsDenominator = f pointsDenominator
    , asstPoints = f asstPoints
    }


toPercents : Model Float -> { grade : Float, percentAsstWorth : Float, asstPoints : Float }
toPercents { pointsNumerator, pointsDenominator, asstPoints } =
    { grade = pointsNumerator / pointsDenominator * 100
    , percentAsstWorth = asstPoints / (asstPoints + pointsDenominator) * 100
    , asstPoints = asstPoints
    }



--UPDATE


type Msg
    = PointsNumerator String
    | PointsDenominator String
    | AsstPoints String


update : Msg -> Model String -> Model String
update msg model =
    case msg of
        PointsNumerator new ->
            { model | pointsNumerator = new }

        PointsDenominator new ->
            { model | pointsDenominator = new }

        AsstPoints new ->
            { model | asstPoints = new }



--VIEW


view : Model String -> Html Msg
view model =
    divClass "card-body"
        [ Html.span []
            [ label [] [ text "Current grade (points)" ]
            , divClass "input-group"
                [ input
                    [ class "numberInput form-control"
                    , onInput PointsNumerator
                    , type_ "number"
                    , value model.pointsNumerator
                    , step "any"
                    ]
                    []
                , divClass1 "input-group-append" <|
                    span [ class "input-group-text" ] [ text "/" ]
                , input
                    [ class "numberInput form-control"
                    , onInput PointsDenominator
                    , type_ "number"
                    , value model.pointsDenominator
                    , step "any"
                    , attribute "aria-label" "Points denominator"
                    ]
                    []
                , divClass1 "input-group-append" <|
                    span [ class "input-group-text" ]
                        -- TODO
                        [ text <| "= " ++ floatToStr (toPercents (map strToFloat model)).grade ++ "%" ]
                ]
            ]
        , numInput False AsstPoints model.asstPoints "Assignment total points: "
        ]
