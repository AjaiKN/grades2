port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, br, button, div, input, table, td, text, th, tr)
import Html.Attributes exposing (attribute, class, step, type_, value)
import Html.Events exposing (onClick, onInput)
import Input.Number
import Round


port plot :
    ( Float -- grade %
    , Float -- % assignment is worth
    )
    -> Cmd msg



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model t =
    --t is either String or Float
    { tab : Tab t
    }


makeModel tab =
    { tab = tab }


type Tab t
    = Percents (PercentsModel t)
    | Points (PointsModel t)


type alias PercentsModel t =
    { grade : t
    , percentAsstWorth : t
    , asstPoints : t
    }


type alias PointsModel t =
    { pointsNumerator : t
    , pointsDenominator : t
    , asstPoints : t
    }


init : () -> ( Model String, Cmd Msg )
init _ =
    ( convertModel floatToStr { tab = Percents { grade = 88, percentAsstWorth = 25, asstPoints = 100 } }, plot ( 88, 25 ) )



--CONVERSION BETWEEN FLOAT AND STRING MODELS


convertModel : (a -> b) -> Model a -> Model b
convertModel f { tab } =
    case tab of
        Percents mod ->
            { tab = Percents (convertPercentsModel f mod)
            }

        Points mod ->
            { tab = Points (convertPointsModel f mod)
            }


convertPercentsModel : (a -> b) -> PercentsModel a -> PercentsModel b
convertPercentsModel f { grade, percentAsstWorth, asstPoints } =
    { grade = f grade
    , percentAsstWorth = f percentAsstWorth
    , asstPoints = f asstPoints
    }


convertPointsModel : (a -> b) -> PointsModel a -> PointsModel b
convertPointsModel f { pointsNumerator, pointsDenominator, asstPoints } =
    { pointsNumerator = f pointsNumerator
    , pointsDenominator = f pointsDenominator
    , asstPoints = f asstPoints
    }


strToFloat : String -> Float
strToFloat =
    String.toFloat >> Maybe.withDefault 0.0


floatToStr : Float -> String
floatToStr =
    Round.roundNum 3 >> String.fromFloat



-- UPDATE


type Msg
    = Grade String
    | PercentAsstWorth String
    | AsstPoints String
    | PointsNumerator String
    | PointsDenominator String
    | TabSwitchPercents
    | TabSwitchPoints
    | DoNothing



--TODO - not used


stringTaker : (Float -> Msg) -> (String -> Msg)
stringTaker f =
    \str ->
        case str of
            "" ->
                f 0

            _ ->
                case String.toFloat str of
                    Nothing ->
                        DoNothing

                    Just val ->
                        f val


update : Msg -> Model String -> ( Model String, Cmd Msg )
update msg { tab } =
    let
        newModel : Model String
        newModel =
            case tab of
                Percents model ->
                    { tab = updatePercents msg model }

                Points model ->
                    { tab = updatePoints msg model }

        percentsModel : PercentsModel Float
        percentsModel =
            newModel |> convertModel strToFloat |> getPercentsModel
    in
    ( newModel
    , plot ( percentsModel.grade, percentsModel.percentAsstWorth )
    )


updatePercents :
    Msg
    -> PercentsModel String
    -> Tab String
updatePercents msg model =
    case msg of
        Grade new ->
            Percents { model | grade = new }

        PercentAsstWorth new ->
            Percents { model | percentAsstWorth = new }

        AsstPoints new ->
            Percents { model | asstPoints = new }

        TabSwitchPoints ->
            convertPercentsModel strToFloat model
                |> Percents
                |> makeModel
                |> getPointsModel
                |> Points
                |> makeModel
                |> convertModel floatToStr
                |> .tab

        _ ->
            Percents model


updatePoints : Msg -> PointsModel String -> Tab String
updatePoints msg model =
    case msg of
        PointsNumerator new ->
            Points { model | pointsNumerator = new }

        PointsDenominator new ->
            Points { model | pointsDenominator = new }

        AsstPoints new ->
            Points { model | asstPoints = new }

        TabSwitchPercents ->
            convertPointsModel strToFloat model
                |> Points
                |> makeModel
                |> getPercentsModel
                |> Percents
                |> makeModel
                |> convertModel floatToStr
                |> .tab

        _ ->
            Points model



-- VIEW


view : Model String -> Html Msg
view model =
    div []
        [ viewHeaders model.tab
        , viewTabContent model
        , table1 (convertModel strToFloat model)
        , table2 (convertModel strToFloat model)
        ]


viewHeaders : Tab String -> Html Msg
viewHeaders tab =
    let
        ( isPercents, isPoints ) =
            case tab of
                Percents _ ->
                    ( True, False )

                Points _ ->
                    ( False, True )
    in
    div [ class "tab" ]
        [ viewTabLink isPercents TabSwitchPercents "Percents"
        , viewTabLink isPoints TabSwitchPoints "Points"
        ]


viewTabLink : Bool -> Msg -> String -> Html Msg
viewTabLink isCurrentTab click label =
    let
        activeClass =
            if isCurrentTab then
                " active"

            else
                ""
    in
    button
        [ class ("tablinks" ++ activeClass)
        , onClick click
        ]
        [ text label ]


viewTabContent : Model String -> Html Msg
viewTabContent model =
    div
        [ class "tabcontent" ]
        (case model.tab of
            Percents percentsModel ->
                [ numInput Grade percentsModel.grade "Current grade (%): "
                , br [] []
                , numInput PercentAsstWorth percentsModel.percentAsstWorth "Percent of grade assignment is worth (%): "
                , br [] []
                , numInput AsstPoints percentsModel.asstPoints "Assignment total points (optional): "
                , br [] []
                ]

            Points pointsModel ->
                [ numInput PointsNumerator pointsModel.pointsNumerator "Current grade (points): "
                , numInput PointsDenominator pointsModel.pointsDenominator " / "
                , text (" = " ++ floatToStr (getPercentsModel (convertModel strToFloat model)).grade ++ "%")
                , br [] []
                , numInput AsstPoints pointsModel.asstPoints "Assignment total points: "
                ]
        )


numInput : (String -> Msg) -> String -> String -> Html Msg
numInput changer val label =
    Html.span []
        [ text label
        , input
            [ class "numberInput"
            , onInput changer
            , type_ "number"
            , value val
            , step "any"
            ]
            []
        ]


{-| Given a model and an assignment grade, returns the new final grade
-}
finalGrade : Model Float -> (Float -> Float)
finalGrade model assignmentGrade =
    let
        mod : PercentsModel Float
        mod =
            getPercentsModel model
    in
    (mod.grade * (1 - mod.percentAsstWorth / 100)) + assignmentGrade * mod.percentAsstWorth / 100


{-| Given a model and the final grade needed, returns the grade needed on the assignment.
(assignmentGradeNeeded model) is the inverse function of (finalGrade model).
-}
assignmentGradeNeeded : Model Float -> (Float -> Float)
assignmentGradeNeeded model finalGradeNeeded =
    let
        mod : PercentsModel Float
        mod =
            getPercentsModel model
    in
    (finalGradeNeeded - mod.grade * (1 - mod.percentAsstWorth / 100)) / (mod.percentAsstWorth / 100)


table1 : Model Float -> Html Msg
table1 model =
    let
        fun =
            finalGrade model

        asstGrades =
            List.map (\n -> toFloat (n * 10)) (List.range 0 11)

        asstPoints =
            List.map (\n -> n / 100 * (getPercentsModel model).asstPoints) asstGrades

        totalGrades =
            List.map fun asstGrades
    in
    makeTable
        [ "Assignment grade (points)", "Assignment grade (%)", "Total grade" ]
        [ asstPoints, asstGrades, totalGrades ]


table2 : Model Float -> Html Msg
table2 model =
    let
        fun =
            assignmentGradeNeeded model

        totalGrades =
            List.map (\n -> toFloat (n * 10)) (List.range 0 11)

        asstGrades =
            List.map fun totalGrades

        --filter out assignments grades < 0
        ( finalAsstGrades, finalTotalGrades ) =
            List.map2 Tuple.pair asstGrades totalGrades
                |> List.filter (\( asst, total ) -> asst >= 0 && asst < 140)
                |> List.unzip

        cols =
            [ List.map (\n -> n / 100 * (getPercentsModel model).asstPoints) finalAsstGrades
            , finalAsstGrades
            , finalTotalGrades
            ]
    in
    makeTable
        [ "Assignment grade (points)", "Assignment grade (%)", "Total grade" ]
        cols



--CONVERTING BETWEEN POINTS AND PERCENTS SYSTEMS


getPointsModel : Model Float -> PointsModel Float
getPointsModel { tab } =
    case tab of
        Percents { grade, percentAsstWorth, asstPoints } ->
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

        Points mod ->
            mod


getPercentsModel : Model Float -> PercentsModel Float
getPercentsModel { tab } =
    case tab of
        Percents mod ->
            mod

        Points { pointsNumerator, pointsDenominator, asstPoints } ->
            { grade = pointsNumerator / pointsDenominator * 100
            , percentAsstWorth = asstPoints / (asstPoints + pointsDenominator) * 100
            , asstPoints = asstPoints
            }



--TABLE HELPERS


makeTable : List String -> List (List Float) -> Html Msg
makeTable headers cols =
    let
        convertedCols =
            List.map (\l -> List.map floatToStr l) cols

        transposed =
            transpose convertedCols
    in
    makeTableFromRows headers transposed


transpose : List (List a) -> List (List a)
transpose ll =
    case ll of
        [] ->
            []

        [] :: xss ->
            transpose xss

        (x :: xs) :: xss ->
            let
                heads =
                    List.filterMap List.head xss

                tails =
                    List.filterMap List.tail xss
            in
            (x :: heads) :: transpose (xs :: tails)


makeTableFromRows : List String -> List (List String) -> Html Msg
makeTableFromRows headers l =
    let
        border : String -> Attribute msg
        border =
            attribute "border"

        --tdth is either td or th
        makeElem tdth s =
            tdth [] [ text s ]

        makeRow tdth r =
            tr [] (List.map (makeElem tdth) r)
    in
    table [ border "1" ] (makeRow th headers :: List.map (makeRow td) l)



-- SUBSCRIPTIONS


subscriptions : Model String -> Sub Msg
subscriptions model =
    Sub.none
