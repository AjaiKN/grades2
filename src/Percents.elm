module Percents exposing (Model, Msg, map, toPoints, update, view)

import Html exposing (Html)
import HtmlHelpers exposing (divClass, numInput)



-- MODEL


type alias Model t =
    { grade : t
    , percentAsstWorth : t
    , asstPoints : t
    }


map : (a -> b) -> Model a -> Model b
map f { grade, percentAsstWorth, asstPoints } =
    { grade = f grade
    , percentAsstWorth = f percentAsstWorth
    , asstPoints = f asstPoints
    }


toPoints : Model Float -> { pointsNumerator : Float, pointsDenominator : Float, asstPoints : Float }
toPoints { grade, percentAsstWorth, asstPoints } =
    let
        denominator =
            asstPoints * (100 / percentAsstWorth - 1)

        numerator =
            grade / 100 * denominator
    in
    { pointsNumerator = numerator
    , pointsDenominator = denominator
    , asstPoints = asstPoints
    }



-- UPDATE


type Msg
    = Grade String
    | PercentAsstWorth String
    | AsstPoints String


update : Msg -> Model String -> Model String
update msg model =
    case msg of
        Grade new ->
            { model | grade = new }

        PercentAsstWorth new ->
            { model | percentAsstWorth = new }

        AsstPoints new ->
            { model | asstPoints = new }



-- VIEW


view : Model String -> Html Msg
view model =
    divClass "card-body"
        [ numInput True Grade model.grade "Current grade (%): "
        , numInput True PercentAsstWorth model.percentAsstWorth "Percent of grade assignment is worth (%): "
        , numInput False AsstPoints model.asstPoints "Assignment total points (optional): "
        ]
