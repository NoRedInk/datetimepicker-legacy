module DatePicker exposing
    ( init, Model
    , view
    )

{-|

@docs init, Model
@docs view

-}

import Css exposing (..)
import Css.Global exposing (descendants)
import DateTimePicker exposing (..)
import DateTimePicker.Config exposing (DatePickerConfig)
import DateTimePicker.DateTime as DateTime
import DateTimePicker.DateUtils
import DateTimePicker.Events exposing (onMouseDownPreventDefault, onTouchStartPreventDefault)
import DateTimePicker.Formatter exposing (accessibilityDateFormatter)
import DateTimePicker.Internal exposing (DateSelection, InternalState(..), StateValue, getStateValue, initialStateValueWithToday)
import DateTimePicker.Styles as Styles
import DateTimePicker.Svg
import Html.Styled as Html exposing (Html, div, span, tbody, td, text, th, thead, tr)
import Html.Styled.Attributes exposing (attribute, css)
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.TextInput.V7 as TextInput
import String
import Time


{-| The state of the date picker.
-}
type alias Model =
    InternalState


{-| Pass in the current date.
-}
init : DateTime.DateTime -> Model
init today =
    InternalState (initialStateValueWithToday today)


{-| -}
view : String -> DatePickerConfig msg -> List (TextInput.Attribute String msg) -> Model -> Maybe DateTime.DateTime -> Html msg
view label config attributes ((InternalState stateValue) as state) currentDate =
    Html.node "date-picker"
        (css [ position relative ] :: config.attributes)
        [ TextInput.view label
            ([ TextInput.onFocus (datePickerFocused config stateValue currentDate)
             , TextInput.onBlur (blurInputHandler config stateValue currentDate)
             , TextInput.onEnter (blurInputHandler config stateValue currentDate)
             , TextInput.text
                (\newValue ->
                    config.onChange (setTextInput newValue stateValue)
                        currentDate
                )
             , TextInput.value stateValue.textInputValue
             ]
                ++ attributes
            )
        , if config.usePicker && stateValue.inputFocused then
            Html.node "date-picker-dialog"
                [ onMouseDownPreventDefault (config.onChange state currentDate)
                , css [ display block, Styles.dialog ]
                ]
                [ datePickerDialog config state currentDate ]

          else
            Html.text ""
        ]


datePickerDialog : DatePickerConfig msg -> Model -> Maybe DateTime.DateTime -> Html msg
datePickerDialog config state currentDate =
    let
        stateValue =
            getStateValue state
    in
    div
        [ css [ float left ] ]
        [ Html.node "date-time-picker-header"
            [ css
                [ boxSizing borderBox
                , displayFlex
                , alignItems center
                , justifyContent spaceBetween
                , padding2 (px 10) (px 7)
                , backgroundColor Colors.gray96
                , Fonts.baseFont
                ]
            ]
            (navigation config state currentDate)
        , calendar config state
        , -- Footer
          Html.node "date-time-picker-footer"
            [ css
                [ display block
                , textAlign center
                , backgroundColor Colors.gray96
                , padding2 (px 7) (px 7)
                , borderTop3 (px 1) solid Colors.gray85
                , height (px 16)
                ]
            ]
            [ stateValue.date |> Maybe.map DateTimePicker.Formatter.footerFormatter |> Maybe.withDefault "--" |> text ]
        ]


navigation : DatePickerConfig msg -> Model -> Maybe DateTime.DateTime -> List (Html msg)
navigation config state currentDate =
    [ previousYearButton config state currentDate
    , previousButton config state currentDate
    , title state
    , nextButton config state currentDate
    , nextYearButton config state currentDate
    ]


title : Model -> Html msg
title state =
    let
        stateValue =
            getStateValue state

        date =
            stateValue.titleDate
    in
    span
        [ css
            [ boxSizing borderBox
            , display inlineBlock
            , width (pct 100)
            , textAlign center
            ]
        ]
        [ date
            |> Maybe.map DateTimePicker.Formatter.titleFormatter
            |> Maybe.withDefault "N/A"
            |> text
        ]


previousYearButton : DatePickerConfig msg -> Model -> Maybe DateTime.DateTime -> Html msg
previousYearButton config state currentDate =
    if config.allowYearNavigation then
        ClickableSvg.button "Previous Year"
            DateTimePicker.Svg.doubleLeftArrow
            [ ClickableSvg.onClick (gotoPreviousYear config state currentDate)
            ]

    else
        Html.text ""


previousButton : DatePickerConfig msg -> Model -> Maybe DateTime.DateTime -> Html msg
previousButton config state currentDate =
    ClickableSvg.button "Previous month"
        DateTimePicker.Svg.leftArrow
        [ ClickableSvg.onClick (gotoPreviousMonth config state currentDate)
        ]


nextButton : DatePickerConfig msg -> Model -> Maybe DateTime.DateTime -> Html msg
nextButton config state currentDate =
    ClickableSvg.button "Next month"
        DateTimePicker.Svg.rightArrow
        [ ClickableSvg.onClick (gotoNextMonth config state currentDate)
        ]


nextYearButton : DatePickerConfig msg -> Model -> Maybe DateTime.DateTime -> Html msg
nextYearButton config state currentDate =
    if config.allowYearNavigation then
        ClickableSvg.button "Next Year"
            DateTimePicker.Svg.doubleRightArrow
            [ ClickableSvg.onClick (gotoNextYear config state currentDate)
            ]

    else
        Html.text ""


calendar : DatePickerConfig msg -> Model -> Html msg
calendar config state =
    let
        stateValue =
            getStateValue state
    in
    case stateValue.titleDate of
        Nothing ->
            Html.text ""

        Just titleDate ->
            let
                month =
                    titleDate.month

                year =
                    titleDate.year

                days =
                    DateTimePicker.DateUtils.generateCalendar
                        config.firstDayOfWeek
                        month
                        year

                header =
                    thead []
                        [ tr
                            []
                            (dayNames config)
                        ]

                matchesDay : Maybe DateTime.DateTime -> DateTimePicker.DateUtils.Day -> Bool
                matchesDay reference day =
                    let
                        date =
                            DateTimePicker.DateUtils.dayToDateTime year month day
                    in
                    reference
                        |> Maybe.map
                            (\current ->
                                (date.day == current.day)
                                    && (date.month == current.month)
                                    && (date.year == current.year)
                            )
                        |> Maybe.withDefault False

                isInRange day =
                    case config.earliestDate of
                        Nothing ->
                            True

                        Just earliestDate ->
                            DateTime.compareDays
                                (DateTimePicker.DateUtils.dayToDateTime year month day)
                                earliestDate
                                /= LT

                toCell day =
                    let
                        selectedDate =
                            DateTimePicker.DateUtils.dayToDateTime year month day

                        styles =
                            List.concat
                                [ case day.monthType of
                                    DateTimePicker.DateUtils.Previous ->
                                        [ color Colors.gray75 ]

                                    DateTimePicker.DateUtils.Current ->
                                        []

                                    DateTimePicker.DateUtils.Next ->
                                        [ color Colors.gray75 ]
                                , if isInRange day then
                                    []

                                  else
                                    [ backgroundColor inherit
                                    , cursor default
                                    , color Colors.gray85
                                    , hover [ backgroundColor inherit ]
                                    ]
                                , if matchesDay stateValue.date day then
                                    [ Styles.highlightStyle
                                    ]

                                  else if matchesDay stateValue.today day then
                                    [ boxShadow6 inset zero zero (Css.px 5) zero Colors.azure
                                    , borderRadius (px 0)
                                    , hover [ backgroundColor Colors.frost ]
                                    ]

                                  else
                                    []
                                ]

                        handler =
                            cellClickHandler config
                                stateValue
                                { year = year, month = month, day = day }

                        handlers =
                            if isInRange day then
                                [ onMouseDownPreventDefault handler
                                , onTouchStartPreventDefault handler
                                ]

                            else
                                []
                    in
                    td
                        (List.concat
                            [ [ css styles
                              , attribute "role" "button"
                              , attribute "aria-label" (accessibilityDateFormatter selectedDate)
                              , attribute "data-in-range" <|
                                    if isInRange day then
                                        "true"

                                    else
                                        "false"
                              ]
                            , handlers
                            ]
                        )
                        [ text <| String.fromInt day.day ]

                toWeekRow week =
                    tr [] (List.map toCell week)

                body =
                    tbody [] (List.map toWeekRow days)
            in
            Html.table
                [ css
                    [ backgroundColor Colors.white
                    , Styles.tableStyle
                    , width auto
                    , margin (px 0)
                    , descendants
                        [ Css.Global.thead
                            []
                        , Css.Global.td
                            [ Styles.cellStyle
                            , textAlign right
                            ]
                        , Css.Global.th
                            [ Styles.dayStyle
                            , backgroundColor Colors.gray96
                            , fontWeight normal
                            , borderBottom3 (px 1) solid Colors.gray85
                            ]
                        ]
                    ]
                ]
                [ header
                , body
                ]


dayNames : DatePickerConfig msg -> List (Html msg)
dayNames config =
    let
        days =
            [ th [] [ text config.nameOfDays.sunday ]
            , th [] [ text config.nameOfDays.monday ]
            , th [] [ text config.nameOfDays.tuesday ]
            , th [] [ text config.nameOfDays.wednesday ]
            , th [] [ text config.nameOfDays.thursday ]
            , th [] [ text config.nameOfDays.friday ]
            , th [] [ text config.nameOfDays.saturday ]
            ]

        shiftAmount =
            DateTimePicker.DateUtils.dayToInt Time.Sun config.firstDayOfWeek
    in
    rotate shiftAmount days


rotate : Int -> List a -> List a
rotate n xs =
    List.drop n xs ++ List.take n xs



-- ACTIONS


gotoNextMonth :
    { config | onChange : Model -> Maybe DateTime.DateTime -> msg }
    -> State
    -> (Maybe DateTime.DateTime -> msg)
gotoNextMonth config state =
    let
        stateValue =
            getStateValue state

        updatedTitleDate =
            Maybe.map (DateTime.addMonths 1) stateValue.titleDate
    in
    config.onChange <| InternalState { stateValue | titleDate = updatedTitleDate }


gotoNextYear :
    { config | onChange : Model -> Maybe DateTime.DateTime -> msg }
    -> State
    -> (Maybe DateTime.DateTime -> msg)
gotoNextYear config state =
    let
        stateValue =
            getStateValue state

        updatedTitleDate =
            Maybe.map (DateTime.addMonths 12) stateValue.titleDate
    in
    config.onChange <| InternalState { stateValue | titleDate = updatedTitleDate }


gotoPreviousMonth :
    { config | onChange : Model -> Maybe DateTime.DateTime -> msg }
    -> State
    -> (Maybe DateTime.DateTime -> msg)
gotoPreviousMonth config state =
    let
        stateValue =
            getStateValue state

        updatedTitleDate =
            Maybe.map (DateTime.addMonths -1) stateValue.titleDate
    in
    config.onChange <| InternalState { stateValue | titleDate = updatedTitleDate }


gotoPreviousYear :
    { config | onChange : Model -> Maybe DateTime.DateTime -> msg }
    -> State
    -> (Maybe DateTime.DateTime -> msg)
gotoPreviousYear config state =
    let
        stateValue =
            getStateValue state

        updatedTitleDate =
            Maybe.map (DateTime.addMonths -12) stateValue.titleDate
    in
    config.onChange <| InternalState { stateValue | titleDate = updatedTitleDate }



-- Cell click handling


cellClickHandler :
    { config
        | onChange : State -> Maybe DateTime.DateTime -> msg
        , toInput : DateTime.DateTime -> String
    }
    -> StateValue
    -> DateSelection
    -> msg
cellClickHandler config stateValue dateSelection =
    let
        selectedDate =
            case stateValue.date of
                Just currentDate ->
                    DateTime.fromParts dateSelection.year
                        dateSelection.month
                        dateSelection.day.day
                        currentDate.hour
                        currentDate.minute

                Nothing ->
                    DateTime.fromDate dateSelection.year dateSelection.month dateSelection.day.day

        adjustedSelectedDate =
            case dateSelection.day.monthType of
                DateTimePicker.DateUtils.Previous ->
                    DateTime.addMonths -1 selectedDate

                DateTimePicker.DateUtils.Next ->
                    DateTime.addMonths 1 selectedDate

                DateTimePicker.DateUtils.Current ->
                    selectedDate

        updatedStateValue =
            { stateValue | date = Just adjustedSelectedDate }
    in
    case dateSelection.day.monthType of
        DateTimePicker.DateUtils.Previous ->
            gotoPreviousMonth config (updateTextInputFromDate config updatedStateValue) (Just adjustedSelectedDate)

        DateTimePicker.DateUtils.Next ->
            gotoNextMonth config (updateTextInputFromDate config updatedStateValue) (Just adjustedSelectedDate)

        DateTimePicker.DateUtils.Current ->
            config.onChange (updateTextInputFromDate config updatedStateValue) (Just adjustedSelectedDate)
