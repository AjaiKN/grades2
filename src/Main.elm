port module Main exposing (Model, Msg(..), PercentsModel, PointsModel, Tab(..), assignmentGradeNeeded, finalGrade, getPercentsModel, getPointsModel, init, main, update, view)

import Browser
import Html exposing (Attribute, Html, a, br, button, div, input, label, nav, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, href, id, scope, step, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Regex
import Round


port plot :
    ( Float -- grade %
    , Float -- % assignment is worth
    )
    -> Cmd msg


port copyToClipboard : String -> Cmd msg



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
    { baseUrl : String
    , tab : Tab t
    }


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


init : ( String, String ) -> ( Model String, Cmd Msg )
init ( url, urlWithoutQuery ) =
    let
        defaultModel =
            mapModel
                floatToStr
                { tab = Percents { grade = 88, percentAsstWorth = 25, asstPoints = 100 }
                , baseUrl = urlWithoutQuery
                }

        mod =
            { defaultModel
                | tab = Maybe.withDefault defaultModel.tab (getModelFromUrl url)
            }

        percentsModel =
            mod |> mapModel strToFloat |> getPercentsModel
    in
    ( mod
    , plot ( percentsModel.grade, percentsModel.percentAsstWorth )
    )


getModelFromUrl : String -> Maybe (Tab String)
getModelFromUrl str =
    case getSubmatches ".*\\?type=percent&grade=(.*)&percentAsstWorth=(.*)&asstPoints=(.*)" str of
        Just [ a, b, c ] ->
            PercentsModel a b c
                |> Percents
                |> Just

        _ ->
            case getSubmatches ".*\\?type=point&numerator=(.*)&denominator=(.*)&asstPoints=(.*)" str of
                Just [ a, b, c ] ->
                    PointsModel a b c
                        |> Points
                        |> Just

                _ ->
                    Nothing


getSubmatches : String -> String -> Maybe (List String)
getSubmatches regexStr s =
    let
        r : Regex.Regex
        r =
            Maybe.withDefault Regex.never <| Regex.fromString regexStr
    in
    Maybe.map (.submatches >> List.map (Maybe.withDefault "")) (List.head (Regex.find r s))


getPageUrl : Model String -> String
getPageUrl { baseUrl, tab } =
    baseUrl
        ++ (case tab of
                Percents { grade, percentAsstWorth, asstPoints } ->
                    "?type=percent&grade=" ++ grade ++ "&percentAsstWorth=" ++ percentAsstWorth ++ "&asstPoints=" ++ asstPoints

                Points { pointsNumerator, pointsDenominator, asstPoints } ->
                    "?type=point&numerator=" ++ pointsNumerator ++ "&denominator=" ++ pointsDenominator ++ "&asstPoints=" ++ asstPoints
           )


getPageLink : Model String -> Html msg
getPageLink model =
    let
        url =
            getPageUrl model
    in
    a [ href url, id "page-link" ] [ text url ]



--CONVERSION BETWEEN MODEL TYPES (Model Float and Model String)


mapModel : (a -> b) -> Model a -> Model b
mapModel f { baseUrl, tab } =
    case tab of
        Percents mod ->
            { baseUrl = baseUrl
            , tab = Percents (mapPercentsModel f mod)
            }

        Points mod ->
            { baseUrl = baseUrl
            , tab = Points (mapPointsModel f mod)
            }


mapPercentsModel : (a -> b) -> PercentsModel a -> PercentsModel b
mapPercentsModel f { grade, percentAsstWorth, asstPoints } =
    { grade = f grade
    , percentAsstWorth = f percentAsstWorth
    , asstPoints = f asstPoints
    }


mapPointsModel : (a -> b) -> PointsModel a -> PointsModel b
mapPointsModel f { pointsNumerator, pointsDenominator, asstPoints } =
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
    = CopyToClipboard
    | Grade String
    | PercentAsstWorth String
    | AsstPoints String
    | PointsNumerator String
    | PointsDenominator String
    | TabSwitchPercents
    | TabSwitchPoints
    | DoNothing


update : Msg -> Model String -> ( Model String, Cmd Msg )
update msg model =
    case msg of
        CopyToClipboard ->
            ( model, copyToClipboard (getPageUrl model) )

        _ ->
            let
                newModel : Model String
                newModel =
                    case model.tab of
                        Percents mod ->
                            { model | tab = updatePercents msg mod }

                        Points mod ->
                            { model | tab = updatePoints msg mod }

                percentsModel : PercentsModel Float
                percentsModel =
                    newModel |> mapModel strToFloat |> getPercentsModel
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
            mapPercentsModel strToFloat model
                |> Percents
                |> Model ""
                |> getPointsModel
                |> Points
                |> Model ""
                |> mapModel floatToStr
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
            mapPointsModel strToFloat model
                |> Points
                |> Model ""
                |> getPercentsModel
                |> Percents
                |> Model ""
                |> mapModel floatToStr
                |> .tab

        _ ->
            Points model



-- VIEW


view : Model String -> Html Msg
view model =
    div []
        [ divClass1 "text-center mt-2 mb-2" <|
            button
                [ type_ "button"
                , class "btn btn-primary"
                , id "copyButton"
                , onClick CopyToClipboard
                , attribute "data-toggle" "tooltip"
                , attribute "data-placement" "bottom"
                , attribute "title" "Copy"
                ]
                [ text "Copy link to this calculation" ]
        , centeredRow <|
            divClass "tabcontent card container-fluid mb-3"
                [ divClass "card-header" [ viewHeaders model.tab ]
                , viewTabContent model
                ]
        , centeredRow <|
            table1 (mapModel strToFloat model)
        , centeredRow <|
            table2 (mapModel strToFloat model)
        ]


centeredRow =
    divClass1 "row justify-content-center"
        << divClass1 "col-lg-6"


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
    nav [ class "nav nav-tabs card-header-tabs" ]
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
    a
        [ class ("nav-link" ++ activeClass)
        , onClick click
        , href "#"
        ]
        [ text label ]


viewTabContent : Model String -> Html Msg
viewTabContent model =
    divClass "card-body"
        (case model.tab of
            Percents percentsModel ->
                [ numInput True Grade percentsModel.grade "Current grade (%): "
                , numInput True PercentAsstWorth percentsModel.percentAsstWorth "Percent of grade assignment is worth (%): "
                , numInput False AsstPoints percentsModel.asstPoints "Assignment total points (optional): "
                ]

            Points pointsModel ->
                -- [ divClass "row mb-0"
                --     [ numInput PointsNumerator pointsModel.pointsNumerator "Current grade (points): "
                --     , numInput PointsDenominator pointsModel.pointsDenominator " / "
                --     , text (" = " ++ floatToStr (getPercentsModel (mapModel strToFloat model)).grade ++ "%")
                --     ]
                [ Html.span []
                    [ label [] [ text "Current grade (points)" ]
                    , divClass "input-group"
                        [ input
                            [ class "numberInput form-control"
                            , onInput PointsNumerator
                            , type_ "number"
                            , value pointsModel.pointsNumerator
                            , step "any"
                            ]
                            []
                        , divClass "input-group-append" [ span [ class "input-group-text" ] [ text "/" ] ]
                        , input
                            [ class "numberInput form-control"
                            , onInput PointsDenominator
                            , type_ "number"
                            , value pointsModel.pointsDenominator
                            , step "any"
                            , attribute "aria-label" "Points denominator"
                            ]
                            []
                        ]
                    ]
                , numInput False AsstPoints pointsModel.asstPoints "Assignment total points: "
                ]
        )


numInput : Bool -> (String -> Msg) -> String -> String -> Html Msg
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
        makeElem s =
            td [] [ text s ]

        makeRow r =
            tr [] (List.map makeElem r)

        body =
            tbody [] <| List.map makeRow l

        head =
            thead [ class "thead-light" ]
                [ tr [] <|
                    List.map (th [ scope "col" ] << List.singleton << text) headers
                ]
    in
    table [ class "table table-striped table-sm" ]
        [ head, body ]



-- SUBSCRIPTIONS


subscriptions : Model String -> Sub Msg
subscriptions model =
    Sub.none
