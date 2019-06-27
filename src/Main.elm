port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, br, button, div, input, table, td, text, th, tr)
import Html.Attributes exposing (attribute, class, step, type_, value)
import Html.Events exposing (onClick, onInput)
import Input.Number
import Round


port plot :
    ( Float, Float ) --grade, percent assignment is worth
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


type alias Model =
    { grade : Float
    , percentAsstWorth : Float
    , asstPoints : Float
    , tab : Tab
    }


type Tab
    = Percents
    | Points


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grade = 88, percentAsstWorth = 25, asstPoints = 100, tab = Percents }, plot ( 88, 25 ) )



-- UPDATE


type Msg
    = Grade Float
    | PercentAsstWorth Float
    | AsstPoints Float
    | PointsNumerator Float
    | PointsDenominator Float
    | TabSwitchPercents
    | TabSwitchPoints
    | DoNothing


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Grade new ->
                    { model | grade = new }

                PercentAsstWorth new ->
                    { model | percentAsstWorth = new }

                AsstPoints new ->
                    { model | asstPoints = new }

                PointsNumerator new ->
                    setPointsNumerator model new

                PointsDenominator new ->
                    setPointsDenominator model new

                TabSwitchPercents ->
                    { model | tab = Percents }

                TabSwitchPoints ->
                    { model | tab = Points }

                DoNothing ->
                    model
    in
    ( newModel
    , plot ( newModel.grade, newModel.percentAsstWorth )
    )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewHeaders model.tab
        , viewTabContent model
        , table1 model
        , table2 model
        ]


viewHeaders : Tab -> Html Msg
viewHeaders tab =
    div [ class "tab" ]
        [ viewTabLink tab Percents TabSwitchPercents "Percents"
        , viewTabLink tab Points TabSwitchPoints "Points"
        ]


viewTabLink : Tab -> Tab -> Msg -> String -> Html Msg
viewTabLink currentTab thisTab click label =
    let
        activeClass =
            if currentTab == thisTab then
                " active"

            else
                ""
    in
    button
        [ class ("tablinks" ++ activeClass)
        , onClick click
        ]
        [ text label ]


viewTabContent : Model -> Html Msg
viewTabContent model =
    div
        [ class "tabcontent" ]
        (case model.tab of
            Percents ->
                [ numInput Grade model.grade "Current grade (%): "
                , br [] []
                , numInput PercentAsstWorth model.percentAsstWorth "Percent of grade assignment is worth (%): "
                , br [] []
                , numInput AsstPoints model.asstPoints "Assignment total points (optional): "
                , br [] []
                ]

            Points ->
                [ numInput PointsNumerator (gradePointsNumerator model) "Current grade (points): "
                , numInput PointsDenominator (gradePointsDenominator model) " / "
                , br [] []
                , numInput AsstPoints model.asstPoints "Assignment total points: "
                ]
        )


numInput : (Float -> Msg) -> Float -> String -> Html Msg
numInput changer val label =
    Html.span []
        [ text label
        , input
            [ class "numberInput"
            , onInput (stringTaker changer)
            , type_ "number"
            , value <|
                if abs val < 0.001 then
                    ""

                else
                    Round.round 3 val
            , step "any"
            ]
            []
        ]


getFun : Model -> (Float -> Float)
getFun model =
    \assignmentGrade ->
        (model.grade * (1 - model.percentAsstWorth / 100)) + assignmentGrade * model.percentAsstWorth / 100


getInverseFun : Model -> (Float -> Float)
getInverseFun model =
    \gradeNeeded ->
        (gradeNeeded - model.grade * (1 - model.percentAsstWorth / 100)) / (model.percentAsstWorth / 100)


table1 : Model -> Html Msg
table1 model =
    let
        fun =
            getFun model

        asstGrades =
            List.map (\n -> toFloat (n * 10)) (List.range 0 11)

        asstPoints =
            List.map (\n -> n / 100 * model.asstPoints) asstGrades

        totalGrades =
            List.map fun asstGrades
    in
    makeTable
        [ "Assignment grade (points)", "Assignment grade (%)", "Total grade" ]
        [ asstPoints, asstGrades, totalGrades ]


table2 : Model -> Html Msg
table2 model =
    let
        fun =
            getInverseFun model

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
            [ List.map (\n -> n / 100 * model.asstPoints) finalAsstGrades
            , finalAsstGrades
            , finalTotalGrades
            ]
    in
    makeTable
        [ "Assignment grade (points)", "Assignment grade (%)", "Total grade" ]
        cols



--CONVERTING TO POINTS SYSTEM


gradePointsNumerator model =
    model.grade
        / 100
        * gradePointsDenominator model


setPointsNumerator model new =
    { model | grade = new / gradePointsDenominator model * 100 }


gradePointsDenominator model =
    model.asstPoints
        * (100 / model.percentAsstWorth - 1)


setPointsDenominator model new =
    { model
        | percentAsstWorth = model.asstPoints / (new + model.asstPoints) * 100
        , grade = gradePointsNumerator model / new * 100
    }



--TABLE HELPERS


makeTable : List String -> List (List Float) -> Html Msg
makeTable headers cols =
    let
        convertedCols =
            List.map (\l -> List.map (Round.roundNum 3 >> String.fromFloat) l) cols

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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
