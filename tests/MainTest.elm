module MainTest exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, floatRange, int, list, string, tuple, tuple3)
import Main exposing (..)
import Percents
import Points
import Round
import Test exposing (..)


percent =
    floatRange 0.1 99.9


close =
    Expect.within (Absolute 0.001)


makeBasicModel =
    Model "" False


expectSamePercentsModel other =
    Expect.all
        [ close other.grade << .grade
        , close other.percentAsstWorth << .percentAsstWorth
        , close other.asstPoints << .asstPoints
        ]


expectSamePointsModel other =
    Expect.all
        [ close other.pointsNumerator << .pointsNumerator
        , close other.pointsDenominator << .pointsDenominator
        , close other.asstPoints << .asstPoints
        ]


suite : Test
suite =
    describe "The Main module"
        [ describe "Switching back and forth between PointsModel Float and PercentsModel Float"
            [ fuzz (tuple3 ( percent, percent, percent )) "Percents to Points and back" <|
                \( grade, percentAsstWorth, asstPoints ) ->
                    let
                        percentModel =
                            Percents.Model grade percentAsstWorth asstPoints
                    in
                    percentModel
                        |> Percents.toPoints
                        |> Points.toPercents
                        |> expectSamePercentsModel percentModel
            , fuzz (tuple3 ( percent, percent, percent )) "Points to Percents and back" <|
                \( pointsNumerator, pointsDenominator, asstPoints ) ->
                    let
                        pointModel =
                            Points.Model pointsNumerator pointsDenominator asstPoints
                    in
                    pointModel
                        |> Points.toPercents
                        |> Percents.toPoints
                        |> expectSamePointsModel pointModel
            , test "basic example" <|
                \_ ->
                    Points.Model 2 3 1
                        |> Points.toPercents
                        |> expectSamePercentsModel (Percents.Model 66.6666666666 25 1)
            ]
        , describe "finalGrade and assignmentGradeNeeded"
            [ fuzz (tuple ( tuple3 ( percent, percent, percent ), percent )) "are inverse functions" <|
                \( ( pointsNumerator, pointsDenominator, asstPoints ), num ) ->
                    let
                        model =
                            makeBasicModel <| PointsTab <| Points.Model pointsNumerator pointsDenominator asstPoints
                    in
                    num
                        |> finalGrade model
                        |> assignmentGradeNeeded model
                        |> close num
            , test "basic example" <|
                \_ ->
                    let
                        model =
                            makeBasicModel <| PercentsTab <| Percents.Model 100 50 1000
                    in
                    25
                        |> finalGrade model
                        |> close ((100 + 25) / 2)
            ]
        ]
