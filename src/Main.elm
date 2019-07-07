port module Main exposing (Model, Msg(..), Tab(..), assignmentGradeNeeded, finalGrade, getModelFromUrl, getPageUrl, getPercentsModel, init, main, mapModel, mapTab, update, view)

import Browser
import Helpers exposing (divClass, divClass1, floatToStr, strToFloat)
import Html exposing (Attribute, Html, a, br, button, div, input, label, nav, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, href, id, scope, step, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Percents
import Points
import Regex
import Round


{-| A port to a JavaScript function (in index.html) that makes the plot
at the bottom.
-}
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


{-| The type parameter t represents the type the Tab
is using to store number inputs (either String or Float).
-}
type alias Model t =
    --t is either String or Float
    { baseUrl : String
    , isTouchDevice : Bool
    , tab : Tab t
    }


{-| Represents the current tab of the input area.
-}
type Tab t
    = PercentsTab (Percents.Model t)
    | PointsTab (Points.Model t)


{-| Maps the model from one type parameter to another.
-}
mapModel : (a -> b) -> Model a -> Model b
mapModel f { baseUrl, isTouchDevice, tab } =
    Model baseUrl isTouchDevice (mapTab f tab)


mapTab : (a -> b) -> Tab a -> Tab b
mapTab f tab =
    case tab of
        PercentsTab mod ->
            PercentsTab (Percents.map f mod)

        PointsTab mod ->
            PointsTab (Points.map f mod)


{-| Get a Percents.Model from a Main.Model
-}
getPercentsModel : Model Float -> Percents.Model Float
getPercentsModel { tab } =
    case tab of
        PercentsTab mod ->
            mod

        PointsTab mod ->
            Points.toPercents mod


init : ( String, String, Bool ) -> ( Model String, Cmd Msg )
init ( url, urlWithoutQuery, isTouchDevice ) =
    let
        defaultModel =
            mapModel
                floatToStr
                { tab = PercentsTab { grade = 88, percentAsstWorth = 25, asstPoints = 100 }
                , baseUrl = urlWithoutQuery
                , isTouchDevice = isTouchDevice
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


{-| Get a model from the query part of the URL.
-}
getModelFromUrl : String -> Maybe (Tab String)
getModelFromUrl str =
    case getSubmatches ".*\\?type=percent&grade=(.*)&percentAsstWorth=(.*)&asstPoints=(.*)" str of
        Just [ a, b, c ] ->
            Percents.Model a b c
                |> PercentsTab
                |> Just

        _ ->
            case getSubmatches ".*\\?type=point&numerator=(.*)&denominator=(.*)&asstPoints=(.*)" str of
                Just [ a, b, c ] ->
                    Points.Model a b c
                        |> PointsTab
                        |> Just

                _ ->
                    Nothing


{-| Get all the submatches of a regex in a String.
-}
getSubmatches : String -> String -> Maybe (List String)
getSubmatches regexStr s =
    let
        r : Regex.Regex
        r =
            Maybe.withDefault Regex.never <| Regex.fromString regexStr
    in
    Maybe.map (.submatches >> List.map (Maybe.withDefault "")) (List.head (Regex.find r s))


{-| Get the URL of a link to this page from the model.
-}
getPageUrl : Model String -> String
getPageUrl { baseUrl, tab } =
    baseUrl
        ++ (case tab of
                PercentsTab { grade, percentAsstWorth, asstPoints } ->
                    "?type=percent&grade=" ++ grade ++ "&percentAsstWorth=" ++ percentAsstWorth ++ "&asstPoints=" ++ asstPoints

                PointsTab { pointsNumerator, pointsDenominator, asstPoints } ->
                    "?type=point&numerator=" ++ pointsNumerator ++ "&denominator=" ++ pointsDenominator ++ "&asstPoints=" ++ asstPoints
           )


{-| Get an Html link to this page from the model.
-}
getPageLink : Model String -> Html msg
getPageLink model =
    let
        url =
            getPageUrl model
    in
    a [ href url, id "page-link" ] [ text url ]



-- UPDATE


type Msg
    = PercentsMsg Percents.Msg
    | PointsMsg Points.Msg
    | TabSwitchPercents
    | TabSwitchPoints


update : Msg -> Model String -> ( Model String, Cmd Msg )
update msg model =
    let
        newModel : Model String
        newModel =
            case ( model.tab, msg ) of
                --Deal with messages for Percents tab by delegating to Percents.update
                ( PercentsTab mod, PercentsMsg percentsMsg ) ->
                    { model | tab = PercentsTab (Percents.update percentsMsg mod) }

                --Deal with messages for Points tab by delegating to Points.update
                ( PointsTab mod, PointsMsg pointsMsg ) ->
                    { model | tab = PointsTab (Points.update pointsMsg mod) }

                --Switch from Percents tab to Points tab.
                ( PercentsTab mod, TabSwitchPoints ) ->
                    { model
                        | tab =
                            Percents.map strToFloat mod
                                |> Percents.toPoints
                                |> Points.map floatToStr
                                |> PointsTab
                    }

                --Switch from Points tab to Percents tab.
                ( PointsTab mod, TabSwitchPercents ) ->
                    { model
                        | tab =
                            Points.map strToFloat mod
                                |> Points.toPercents
                                |> Percents.map floatToStr
                                |> PercentsTab
                    }

                --If the message and the model type don't match, do nothing.
                ( _, _ ) ->
                    model

        percentsModel : Percents.Model Float
        percentsModel =
            newModel |> mapModel strToFloat |> getPercentsModel
    in
    ( newModel
      --Redo the plot for every update.
    , plot ( percentsModel.grade, percentsModel.percentAsstWorth )
    )



-- VIEW


view : Model String -> Html Msg
view model =
    div []
        [ --Copy button
          divClass1 "text-center mt-2 mb-2" <|
            button
                [ type_ "button"
                , class "btn btn-primary"
                , id "copyButton"
                , attribute "data-toggle" "tooltip"
                , attribute "data-placement" "bottom"
                , attribute "title" <|
                    if model.isTouchDevice then
                        "Copied"

                    else
                        "Copy"
                , attribute "data-clipboard-text" (getPageUrl model)
                ]
                [ text "Copy link to this calculation" ]

        --Header for tabs
        , centeredRow <|
            divClass "tabcontent card container-fluid mb-3"
                [ divClass "card-header" [ viewHeaders model.tab ]
                , viewTabContent model
                ]

        --Table #1
        , centeredRow <|
            table1 (mapModel strToFloat model)

        --Table #2
        , centeredRow <|
            table2 (mapModel strToFloat model)
        ]


{-| Center some Html with a Bootstrap row.
-}
centeredRow : Html msg -> Html msg
centeredRow =
    divClass1 "row justify-content-center"
        << divClass1 "col-lg-6"


{-| View headers for tabs.
-}
viewHeaders : Tab String -> Html Msg
viewHeaders tab =
    let
        ( isPercents, isPoints ) =
            case tab of
                PercentsTab _ ->
                    ( True, False )

                PointsTab _ ->
                    ( False, True )
    in
    nav [ class "nav nav-tabs card-header-tabs" ]
        [ viewTabLink isPercents TabSwitchPercents "Percents"
        , viewTabLink isPoints TabSwitchPoints "Points"
        ]


viewTabLink : Bool -> msg -> String -> Html msg
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


{-| View tab content by delegating to Percents.view and Points.view
-}
viewTabContent : Model String -> Html Msg
viewTabContent { tab } =
    case tab of
        PercentsTab model ->
            Html.map PercentsMsg (Percents.view model)

        PointsTab model ->
            Html.map PointsMsg (Points.view model)


{-| Given a model and an assignment grade, returns the new final grade.
-}
finalGrade : Model Float -> (Float -> Float)
finalGrade model assignmentGrade =
    let
        mod : Percents.Model Float
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
        mod : Percents.Model Float
        mod =
            getPercentsModel model
    in
    (finalGradeNeeded - mod.grade * (1 - mod.percentAsstWorth / 100)) / (mod.percentAsstWorth / 100)


{-| Table #1: Get total grades for round-number assignment grades.
-}
table1 : Model Float -> Html msg
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


{-| Table #2: Get assignment grades needed for round-number total grades.
-}
table2 : Model Float -> Html msg
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



--TABLE HELPERS


{-| Make an Html table from a List of header names and a List of numerical columns.
-}
makeTable : List String -> List (List Float) -> Html msg
makeTable headers cols =
    let
        convertedCols =
            List.map (\l -> List.map floatToStr l) cols

        transposed =
            transpose convertedCols
    in
    makeTableFromRows headers transposed


{-| Transpose a 2D List.
-}
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


{-| Make an Html table from a List of header names and a List of numerical rows.
-}
makeTableFromRows : List String -> List (List String) -> Html msg
makeTableFromRows headers l =
    let
        border : String -> Attribute msg
        border =
            attribute "border"

        --tdth is either td or th
        makeElem s =
            td [] [ text s ]

        makeRow r =
            tr [ class (colorClassOfRow r) ] (List.map makeElem r)

        body =
            tbody [] <| List.map makeRow l

        head =
            thead [ class "thead-light" ]
                [ tr [] <|
                    List.map (th [ scope "col" ] << List.singleton << text) headers
                ]
    in
    table [ class "table table-sm" ]
        [ head, body ]


{-| Get the name of the CSS class for the color that this row needs to be
based on the letter grade of the final grade (see css/styles.css).
-}
colorClassOfRow : List String -> String
colorClassOfRow l =
    case l of
        [ _, _, grade ] ->
            case String.toFloat grade of
                Just n ->
                    "color-" ++ colorClassOfGrade n

                Nothing ->
                    ""

        _ ->
            ""


colorClassOfGrade : Float -> String
colorClassOfGrade n =
    if n < 60 then
        "f"

    else if n < 70 then
        "d"

    else if n < 80 then
        "c"

    else if n < 90 then
        "b"

    else
        "a"



-- SUBSCRIPTIONS


subscriptions : Model String -> Sub Msg
subscriptions model =
    Sub.none
