module MainTest exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Helpers exposing (floatToStr, strToFloat)
import Main exposing (..)
import Percents
import Points
import Round
import Test exposing (..)


fuzzPercent : Fuzzer Float
fuzzPercent =
    Fuzz.floatRange 0.1 99.9


expectClose : Float -> Float -> Expectation
expectClose =
    Expect.within (Absolute 0.001)


makeBasicModel : Tab t -> Model t
makeBasicModel =
    Model "" False


expectSamePercentsModel : Percents.Model Float -> Percents.Model Float -> Expectation
expectSamePercentsModel other =
    Expect.all
        [ expectClose other.grade << .grade
        , expectClose other.percentAsstWorth << .percentAsstWorth
        , expectClose other.asstPoints << .asstPoints
        ]


expectSamePointsModel : Points.Model Float -> Points.Model Float -> Expectation
expectSamePointsModel other =
    Expect.all
        [ expectClose other.pointsNumerator << .pointsNumerator
        , expectClose other.pointsDenominator << .pointsDenominator
        , expectClose other.asstPoints << .asstPoints
        ]


fuzzPercentsModel : Fuzzer (Percents.Model Float)
fuzzPercentsModel =
    Fuzz.map3 Percents.Model fuzzPercent fuzzPercent fuzzPercent


fuzzPointsModel : Fuzzer (Points.Model Float)
fuzzPointsModel =
    Fuzz.map3 Points.Model fuzzPercent fuzzPercent fuzzPercent


fuzzFloatTab : Fuzz.Fuzzer (Tab Float)
fuzzFloatTab =
    Fuzz.oneOf
        [ Fuzz.map PercentsTab fuzzPercentsModel
        , Fuzz.map PointsTab fuzzPointsModel
        ]


fuzzStrTab : Fuzz.Fuzzer (Tab String)
fuzzStrTab =
    Fuzz.map (mapTab floatToStr) fuzzFloatTab


fuzzFloatModel : Fuzz.Fuzzer (Model Float)
fuzzFloatModel =
    Fuzz.map makeBasicModel fuzzFloatTab


suite : Test
suite =
    describe "The whole program"
        [ describe "Switching back and forth between Points.Model Float and Percents.Model Float"
            [ fuzz fuzzPercentsModel "Percents to Points and back" <|
                \percentsModel ->
                    percentsModel
                        |> Percents.toPoints
                        |> Points.toPercents
                        |> expectSamePercentsModel percentsModel
            , fuzz fuzzPointsModel "Points to Percents and back" <|
                \pointsModel ->
                    pointsModel
                        |> Points.toPercents
                        |> Percents.toPoints
                        |> expectSamePointsModel pointsModel
            , test "basic example" <|
                \_ ->
                    Points.Model 2 3 1
                        |> Points.toPercents
                        |> expectSamePercentsModel (Percents.Model 66.6666666666 25 1)
            ]
        , describe "mapModel"
            [ test "basic example" <|
                \_ ->
                    mapModel floatToStr (Model "" False (PercentsTab (Percents.Model 1 2 3)))
                        |> Expect.equal (Model "" False (PercentsTab (Percents.Model "1" "2" "3")))
            ]
        , describe "getModelFromUrl and getPageUrl"
            [ fuzz fuzzStrTab "are inverses (not including base URL)" <|
                \tab ->
                    tab
                        |> makeBasicModel
                        |> getPageUrl
                        |> getModelFromUrl
                        |> Expect.equal (Just tab)
            ]
        , describe "finalGrade and assignmentGradeNeeded"
            [ fuzz (Fuzz.tuple ( fuzzFloatModel, fuzzPercent )) "are inverse functions" <|
                \( model, num ) ->
                    num
                        |> finalGrade model
                        |> assignmentGradeNeeded model
                        |> expectClose num
            , test "basic example" <|
                \_ ->
                    let
                        model =
                            makeBasicModel <| PercentsTab <| Percents.Model 100 50 1000
                    in
                    finalGrade model 25
                        |> expectClose ((100 + 25) / 2)
            ]
        ]
