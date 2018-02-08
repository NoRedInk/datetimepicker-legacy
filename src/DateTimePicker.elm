module DateTimePicker
    exposing
        ( State
        , datePicker
        , datePickerWithConfig
        , dateTimePicker
        , dateTimePickerWithConfig
        , initialCmd
        , initialState
        , initialStateWithToday
        , timePicker
        , timePickerWithConfig
        )

{-| DateTime Picker


# View

@docs datePicker, datePickerWithConfig, dateTimePicker, dateTimePickerWithConfig, timePicker, timePickerWithConfig


# Initial

@docs initialState, initialStateWithToday, initialCmd


# Internal State

@docs State

-}

import Css exposing (..)
import Css.Foreign exposing (Snippet, children, descendants, withClass)
import Date exposing (Date)
import Date.Extra.Core
import Date.Extra.Duration
import DateTimePicker.AnalogClock
import DateTimePicker.ClockUtils
import DateTimePicker.Config exposing (Config, DatePickerConfig, TimePickerConfig, TimePickerType(..), Type(..), defaultDatePickerConfig, defaultDateTimePickerConfig, defaultTimePickerConfig)
import DateTimePicker.DateUtils
import DateTimePicker.Events exposing (onBlurWithChange, onMouseDownPreventDefault, onMouseUpPreventDefault, onTouchEndPreventDefault, onTouchStartPreventDefault)
import DateTimePicker.Formatter exposing (accessibilityDateFormatter)
import DateTimePicker.Helpers exposing (updateCurrentDate, updateTimeIndicator)
import DateTimePicker.Internal exposing (InternalState(..), StateValue, Time, getStateValue, initialStateValue, initialStateValueWithToday)
import DateTimePicker.Styles as Styles
import DateTimePicker.Svg
import Html.Styled as Html exposing (Html, button, div, input, li, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Styled.Attributes exposing (attribute, css, value)
import Html.Styled.Events exposing (onBlur, onClick, onFocus)
import String
import Task
import Time


-- MODEL


{-| The state of the date time picker (for Internal Use)
-}
type alias State =
    InternalState


{-| Initial state of the DatePicker
-}
initialState : State
initialState =
    InternalState
        initialStateValue


{-| Initial state of the DatePicker with today Date
-}
initialStateWithToday : Date.Date -> State
initialStateWithToday today =
    InternalState
        (initialStateValueWithToday today)


{-| Initial Cmd to set the initial month to be displayed in the datepicker to the current month.
-}
initialCmd : (State -> Maybe Date -> msg) -> State -> Cmd msg
initialCmd onChange state =
    let
        stateValue =
            getStateValue state

        setDate now =
            InternalState
                { stateValue
                    | today = Just now
                    , titleDate = Just <| Date.Extra.Core.toFirstOfMonth now
                }
    in
    Task.perform
        ((setDate >> onChange |> flip) Nothing)
        Date.now



-- ACTIONS


switchMode : Config a msg -> State -> (Maybe Date -> msg)
switchMode config state =
    let
        stateValue =
            getStateValue state
    in
    config.onChange <| InternalState { stateValue | event = "title" }


gotoNextMonth : Config a msg -> State -> (Maybe Date -> msg)
gotoNextMonth config state =
    let
        stateValue =
            getStateValue state

        updatedTitleDate =
            Maybe.map (Date.Extra.Duration.add Date.Extra.Duration.Month 1) stateValue.titleDate
    in
    config.onChange <| InternalState { stateValue | event = "next", titleDate = updatedTitleDate }


gotoNextYear : Config a msg -> State -> (Maybe Date -> msg)
gotoNextYear config state =
    let
        stateValue =
            getStateValue state

        updatedTitleDate =
            Maybe.map (Date.Extra.Duration.add Date.Extra.Duration.Year 1) stateValue.titleDate
    in
    config.onChange <| InternalState { stateValue | event = "nextYear", titleDate = updatedTitleDate }


gotoPreviousMonth : Config a msg -> State -> (Maybe Date -> msg)
gotoPreviousMonth config state =
    let
        stateValue =
            getStateValue state

        updatedTitleDate =
            Maybe.map (Date.Extra.Duration.add Date.Extra.Duration.Month -1) stateValue.titleDate
    in
    config.onChange <| InternalState { stateValue | event = "previous", titleDate = updatedTitleDate }


gotoPreviousYear : Config a msg -> State -> (Maybe Date -> msg)
gotoPreviousYear config state =
    let
        stateValue =
            getStateValue state

        updatedTitleDate =
            Maybe.map (Date.Extra.Duration.add Date.Extra.Duration.Year -1) stateValue.titleDate
    in
    config.onChange <| InternalState { stateValue | event = "previousYear", titleDate = updatedTitleDate }



-- VIEWS


{-| Date Picker view function with default configuration.

Example:
type alias Model = { datePickerState : DateTimePicker.State, value : Maybe Date }

    type Msg
        = DatePickerChanged DateTimePicker.State (Maybe Date)

    view =
        DateTimePicker.datePicker
            DatePickerChanged
            [ class "my-datepicker" ]
            model.datePickerState
            model.value

-}
datePicker : (State -> Maybe Date -> msg) -> List (Html.Attribute msg) -> State -> Maybe Date -> Html msg
datePicker onChange =
    datePickerWithConfig (defaultDatePickerConfig onChange)


{-| Date Picker view function with custom configuration.

Example:
type alias Model = { datePickerState : DateTimePicker.State, value : Maybe Date }

    type Msg
        = DatePickerChanged DateTimePicker.State (Maybe Date)

    customConfig =
        let
            default =
                DateTimePicker.defaultConfig DatePickerChanged
        in
        { default
            | firstDayOfWeek = Date.Mon
            , autoClose = True
        }

    view =
        DateTimePicker.datePickerWithConfig
            customConfig
            DateTimePicker.defaultDatePickerConfig
            [ class "my-datepicker" ]
            model.datePickerState
            model.value

-}
datePickerWithConfig : Config (DatePickerConfig {}) msg -> List (Html.Attribute msg) -> State -> Maybe Date -> Html msg
datePickerWithConfig config =
    view (DateType config)


{-| Date and Time Picker view with default configuration
Example:
type alias Model = { dateTimePickerState : DateTimePicker.State, value : Maybe DateType }

    type Msg
        = DatePickerChanged DateTimePicker.State (Maybe Date)

    view =
        DateTimePicker.dateTimePicker
            DatePickerChanged
            [ class "my-datetimepicker" ]
            model.dateTimePickerState
            model.value

-}
dateTimePicker : (State -> Maybe Date -> msg) -> List (Html.Attribute msg) -> State -> Maybe Date -> Html msg
dateTimePicker onChange =
    dateTimePickerWithConfig (defaultDateTimePickerConfig onChange)


{-| Time Picker view with default configuration
Example:
type alias Model = { timePickerState : DateTimePicker.State, value : Maybe DateType }

    type Msg
        = TimePickerChanged DateTimePicker.State (Maybe Date)

    view =
        DateTimePicker.timePicker
            TimePickerChanged
            [ class "my-timepicker" ]
            model.timePickerState
            model.value

-}
timePicker : (State -> Maybe Date -> msg) -> List (Html.Attribute msg) -> State -> Maybe Date -> Html msg
timePicker onChange =
    timePickerWithConfig (defaultTimePickerConfig onChange)


{-| Date and Time Picker view with custom configuration
Example:
type alias Model = { dateTimePickerState : DateTimePicker.State, value : Maybe Date }

    type Msg
        = DatePickerChanged DateTimePicker.State (Maybe Date)

    customConfig =
        let
            default =
                DateTimePicker.defaultDateTimePickerConfig DatePickerChanged
        in
        { default
            | firstDayOfWeek = Date.Mon
            , autoClose = True
        }

    view =
        DateTimePicker.dateTimePickerWithConfig
            customConfig
            [ class "my-datetimepicker" ]
            model.dateTimePickerState
            model.value

-}
dateTimePickerWithConfig : Config (DatePickerConfig TimePickerConfig) msg -> List (Html.Attribute msg) -> State -> Maybe Date -> Html msg
dateTimePickerWithConfig config =
    view (DateTimeType config)


{-| Time Picker view with custom configuration
Example:
type alias Model = { timePickerState : DateTimePicker.State, value : Maybe Date }

    type Msg
        = TimePickerChanged DateTimePicker.State (Maybe Date)

    customConfig =
        let
            default =
                DateTimePicker.defaultTimePickerConfig TimePickerChanged
        in
        { default
            | autoClose = True
        }

    view =
        DateTimePicker.timePickerWithConfig
            customConfig
            [ class "my-datetimepicker" ]
            model.timePickerState
            model.value

-}
timePickerWithConfig : Config TimePickerConfig msg -> List (Html.Attribute msg) -> State -> Maybe Date -> Html msg
timePickerWithConfig config =
    view (TimeType config)


view : Type msg -> List (Html.Attribute msg) -> State -> Maybe Date -> Html msg
view pickerType attributes state currentDate =
    let
        stateValue =
            getStateValue state

        timeFormatter dateTimePickerConfig =
            dateTimePickerConfig.timeFormatter

        inputAttributes config =
            attributes
                ++ [ onFocus (datePickerFocused pickerType config stateValue currentDate)
                   , onBlurWithChange
                        config.i18n.inputFormat.inputParser
                        (inputChangeHandler config stateValue currentDate)
                   , currentDate
                        |> Maybe.map config.i18n.inputFormat.inputFormatter
                        |> Maybe.withDefault ""
                        |> value
                   ]

        shouldForceClose config =
            config.autoClose && stateValue.forceClose

        html config cssClasses =
            div
                (cssClasses :: config.attributes)
                [ input (inputAttributes config) []
                , if config.usePicker && stateValue.inputFocused && not (shouldForceClose config) then
                    dialog pickerType state currentDate
                  else
                    Html.text ""
                ]
    in
    case pickerType of
        DateType config ->
            html config (css [ position relative ])

        DateTimeType config ->
            html config (css [ position relative ])

        TimeType config ->
            html config (css [])



-- VIEW HELPERSs


dialog : Type msg -> State -> Maybe Date -> Html msg
dialog pickerType state currentDate =
    let
        stateValue =
            getStateValue state

        attributes config =
            [ onMouseDownPreventDefault <| config.onChange (InternalState { stateValue | event = "dialog.onMouseDownPreventDefault" }) currentDate
            , css
                [ fontFamilies [ "Arial", "Helvetica", "sans-serif" ]
                , fontSize (px 14)
                , Styles.borderBoxStyle
                , position absolute
                , border3 (px 1) solid Styles.darkGray
                , boxShadow4 (px 0) (px 5) (px 10) (rgba 0 0 0 0.2)
                , children timePickerDialog
                , property "z-index" "1"
                , displayFlex
                ]
            ]

        withTimeAttributes config timePickerType =
            case timePickerType of
                Analog ->
                    (onClick <| onChangeHandler pickerType stateValue currentDate) :: attributes config

                Digital ->
                    attributes config
    in
    case pickerType of
        DateType datePickerConfig ->
            div (attributes datePickerConfig) [ datePickerDialog pickerType state currentDate ]

        TimeType timePickerConfig ->
            div (withTimeAttributes timePickerConfig timePickerConfig.timePickerType) [ timePickerDialog pickerType state currentDate ]

        DateTimeType timePickerConfig ->
            div (withTimeAttributes timePickerConfig timePickerConfig.timePickerType)
                [ datePickerDialog pickerType state currentDate
                , timePickerDialog pickerType state currentDate
                ]


datePickerDialog : Type msg -> State -> Maybe Date -> Html msg
datePickerDialog pickerType state currentDate =
    let
        stateValue =
            getStateValue state

        html config =
            div
                [ css
                    [ float left
                    , children datePickerDialogCss
                    ]
                ]
                [ div [ css [ Styles.headerStyle ] ]
                    (navigation config state currentDate)
                , calendar pickerType state currentDate
                , -- Footer
                  div
                    [ css
                        [ textAlign center
                        , backgroundColor Styles.lightGray
                        , padding2 (px 7) (px 7)
                        , borderTop3 (px 1) solid Styles.darkGray
                        , height (px 16)
                        ]
                    ]
                    [ stateValue.date |> Maybe.map config.i18n.footerFormatter |> Maybe.withDefault "--" |> text ]
                ]
    in
    case pickerType of
        DateType config ->
            html config

        DateTimeType config ->
            html config

        TimeType _ ->
            text ""


navigation : DatePickerConfig (Config config msg) -> State -> Maybe Date -> List (Html msg)
navigation config state currentDate =
    [ previousYearButton config state currentDate
    , previousButton config state currentDate
    , title config state currentDate
    , nextButton config state currentDate
    , nextYearButton config state currentDate
    ]


title : DatePickerConfig (Config config msg) -> State -> Maybe Date -> Html msg
title config state currentDate =
    let
        stateValue =
            getStateValue state

        date =
            stateValue.titleDate
    in
    span
        [ css
            [ Styles.borderBoxStyle
            , display inlineBlock
            , width (pct 100)
            , textAlign center
            ]
        , onMouseDownPreventDefault <| switchMode config state currentDate
        ]
        [ date
            |> Maybe.map config.i18n.titleFormatter
            |> Maybe.withDefault "N/A"
            |> text
        ]


previousYearButton : DatePickerConfig (Config config msg) -> State -> Maybe Date -> Html msg
previousYearButton config state currentDate =
    if config.allowYearNavigation then
        span
            [ css
                [ Styles.arrowStyle
                , left (px 0)
                ]
            , onMouseDownPreventDefault <| gotoPreviousYear config state currentDate
            , onTouchStartPreventDefault <| gotoPreviousYear config state currentDate
            ]
            [ DateTimePicker.Svg.doubleLeftArrow ]
    else
        Html.text ""


noYearNavigationStyle : DatePickerConfig (Config config msg) -> Css.Style
noYearNavigationStyle config =
    if config.allowYearNavigation then
        Css.batch []
    else
        left (px 0)


previousButton : DatePickerConfig (Config config msg) -> State -> Maybe Date -> Html msg
previousButton config state currentDate =
    span
        [ css
            [ Styles.arrowStyle
            , left (px 22)
            , noYearNavigationStyle config
            ]
        , onMouseDownPreventDefault <| gotoPreviousMonth config state currentDate
        , onTouchStartPreventDefault <| gotoPreviousMonth config state currentDate
        ]
        [ DateTimePicker.Svg.leftArrow ]


nextButton : DatePickerConfig (Config config msg) -> State -> Maybe Date -> Html msg
nextButton config state currentDate =
    span
        [ css
            [ Styles.arrowStyle
            , right (px 22)
            , noYearNavigationStyle config
            ]
        , onMouseDownPreventDefault <| gotoNextMonth config state currentDate
        , onTouchStartPreventDefault <| gotoNextMonth config state currentDate
        ]
        [ DateTimePicker.Svg.rightArrow ]


nextYearButton : DatePickerConfig (Config config msg) -> State -> Maybe Date -> Html msg
nextYearButton config state currentDate =
    if config.allowYearNavigation then
        span
            [ css
                [ Styles.arrowStyle
                , right (px 0)
                ]
            , onMouseDownPreventDefault <| gotoNextYear config state currentDate
            , onTouchStartPreventDefault <| gotoNextYear config state currentDate
            ]
            [ DateTimePicker.Svg.doubleRightArrow ]
    else
        Html.text ""


timePickerDialog : Type msg -> State -> Maybe Date -> Html msg
timePickerDialog pickerType state currentDate =
    let
        html config =
            case config.timePickerType of
                Digital ->
                    digitalTimePickerDialog pickerType state currentDate

                Analog ->
                    analogTimePickerDialog pickerType state currentDate
    in
    case pickerType of
        DateType _ ->
            text ""

        DateTimeType config ->
            html config

        TimeType config ->
            html config


digitalTimePickerDialog : Type msg -> State -> Maybe Date -> Html msg
digitalTimePickerDialog pickerType state currentDate =
    let
        stateValue =
            getStateValue state

        toListItem str =
            li [] [ text str ]

        hours =
            List.range stateValue.hourPickerStart (stateValue.hourPickerStart + 6)

        minutes =
            List.range stateValue.minutePickerStart (stateValue.minutePickerStart + 6)

        ampmList =
            [ "AM", "PM" ]

        timeSelector =
            List.map3 toRow hours minutes (ampmList ++ List.repeat 4 "")

        toRow hour min ampm =
            tr []
                [ hourCell hour
                , minuteCell min
                , amPmCell ampm
                ]

        hourCell hour =
            td
                [ onMouseDownPreventDefault <| hourClickHandler pickerType stateValue hour
                , onTouchStartPreventDefault <| hourClickHandler pickerType stateValue hour
                , case stateValue.time.hour of
                    Just stateHour ->
                        if stateHour == hour then
                            css [ Styles.highlightStyle, hover [ Styles.highlightStyle ] ]
                        else
                            css []

                    Nothing ->
                        css []
                ]
                [ text <| (toString >> DateTimePicker.DateUtils.padding) hour ]

        minuteCell minute =
            td
                [ onMouseDownPreventDefault <| minuteClickHandler pickerType stateValue minute
                , onTouchStartPreventDefault <| minuteClickHandler pickerType stateValue minute
                , case stateValue.time.minute of
                    Just stateMinute ->
                        if stateMinute == minute then
                            css [ Styles.highlightStyle, hover [ Styles.highlightStyle ] ]
                        else
                            css []

                    Nothing ->
                        css []
                ]
                [ text <| (toString >> DateTimePicker.DateUtils.padding) minute ]

        amPmCell ampm =
            let
                defaultStyles =
                    if String.isEmpty ampm then
                        css [ Styles.emptyCellStyle ]
                    else
                        css []
            in
            td
                [ (case stateValue.time.amPm of
                    Just stateAmPm ->
                        if stateAmPm == ampm then
                            css [ Styles.highlightStyle, hover [ Styles.highlightStyle ] ]
                        else
                            defaultStyles

                    Nothing ->
                        defaultStyles
                  )
                    :: (if String.isEmpty ampm then
                            []
                        else
                            [ onMouseDownPreventDefault <| amPmClickHandler pickerType stateValue ampm
                            , onTouchStartPreventDefault <| amPmClickHandler pickerType stateValue ampm
                            ]
                       )
                ]
                [ text ampm ]

        upArrows config =
            [ tr
                [ css
                    [ backgroundColor Styles.lightGray
                    , children
                        [ td [ borderBottom3 (px 1) solid Styles.darkGray ]
                        ]
                    ]
                ]
                [ td
                    [ onMouseDownPreventDefault <| hourUpHandler config stateValue currentDate
                    , onTouchStartPreventDefault <| hourUpHandler config stateValue currentDate
                    ]
                    [ DateTimePicker.Svg.upArrow ]
                , td
                    [ onMouseDownPreventDefault <| minuteUpHandler config stateValue currentDate
                    , onTouchStartPreventDefault <| minuteUpHandler config stateValue currentDate
                    ]
                    [ DateTimePicker.Svg.upArrow ]
                , td [] []
                ]
            ]

        downArrows config =
            [ tr
                [ css
                    [ backgroundColor Styles.lightGray
                    , children [ td [ borderTop3 (px 1) solid Styles.darkGray ] ]
                    ]
                ]
                [ td
                    [ onMouseDownPreventDefault <| hourDownHandler config stateValue currentDate
                    , onTouchStartPreventDefault <| hourDownHandler config stateValue currentDate
                    ]
                    [ DateTimePicker.Svg.downArrow ]
                , td
                    [ onMouseDownPreventDefault <| minuteDownHandler config stateValue currentDate
                    , onTouchStartPreventDefault <| minuteDownHandler config stateValue currentDate
                    ]
                    [ DateTimePicker.Svg.downArrow ]
                , td [] []
                ]
            ]

        html config =
            div [ css [ Styles.timePickerDialog ] ]
                [ div [ css [ Styles.headerStyle ] ]
                    [ Maybe.map config.i18n.timeTitleFormatter currentDate |> Maybe.withDefault "-- : --" |> text ]
                , div
                    [ css
                        [ [ backgroundColor (hex "#fff")
                          , descendants
                                [ Css.Foreign.table
                                    [ Styles.tableStyle
                                    , width (px 120)
                                    , descendants
                                        [ tr [ verticalAlign top ]
                                        , td
                                            [ width (pct 33)
                                            , Styles.cellStyle
                                            , hover
                                                [ backgroundColor Styles.highlightedDay
                                                , Styles.highlightBorderStyle
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                          ]
                        ]
                    ]
                    [ Html.table []
                        [ tbody []
                            (upArrows config
                                ++ timeSelector
                                ++ downArrows config
                            )
                        ]
                    ]
                ]
    in
    case pickerType of
        DateType _ ->
            text ""

        DateTimeType config ->
            html config

        TimeType config ->
            html config


analogTimePickerDialog : Type msg -> State -> Maybe Date -> Html msg
analogTimePickerDialog pickerType state currentDate =
    let
        stateValue =
            getStateValue state

        isActive timeIndicator =
            if stateValue.activeTimeIndicator == Just timeIndicator then
                [ Styles.activeStyle ]
            else
                []

        html config =
            div [ css [ Styles.timePickerDialog, width (px 230) ] ]
                [ div [ css [ Styles.headerStyle, fontSize (Css.em 1.2) ] ]
                    [ span
                        [ onMouseDownPreventDefault (timeIndicatorHandler config stateValue currentDate DateTimePicker.Internal.HourIndicator)
                        , onTouchStartPreventDefault (timeIndicatorHandler config stateValue currentDate DateTimePicker.Internal.HourIndicator)
                        , css [ Styles.timeHeaderStyle ]
                        , class (isActive DateTimePicker.Internal.HourIndicator)
                        ]
                        [ text (stateValue.time.hour |> Maybe.map (toString >> DateTimePicker.DateUtils.padding) |> Maybe.withDefault "--") ]
                    , span [] [ text " : " ]
                    , span
                        [ onMouseDownPreventDefault (timeIndicatorHandler config stateValue currentDate DateTimePicker.Internal.MinuteIndicator)
                        , onTouchStartPreventDefault (timeIndicatorHandler config stateValue currentDate DateTimePicker.Internal.MinuteIndicator)
                        , css [ Styles.timeHeaderStyle ]
                        , class (isActive DateTimePicker.Internal.MinuteIndicator)
                        ]
                        [ text (stateValue.time.minute |> Maybe.map (toString >> DateTimePicker.DateUtils.padding) |> Maybe.withDefault "--") ]
                    , span
                        [ onMouseDownPreventDefault (timeIndicatorHandler config stateValue currentDate DateTimePicker.Internal.AMPMIndicator)
                        , onTouchStartPreventDefault (timeIndicatorHandler config stateValue currentDate DateTimePicker.Internal.AMPMIndicator)
                        , css [ Styles.timeHeaderStyle ]
                        , class (isActive DateTimePicker.Internal.AMPMIndicator)
                        ]
                        [ text (stateValue.time.amPm |> Maybe.withDefault "--") ]
                    ]
                , div
                    [ css
                        [ backgroundColor (hex "#fff")
                        , padding2 (px 12) (px 15)
                        , height (px 202)
                        ]
                    ]
                    [ case stateValue.activeTimeIndicator of
                        Just DateTimePicker.Internal.AMPMIndicator ->
                            amPmPicker config

                        _ ->
                            DateTimePicker.AnalogClock.clock pickerType config.onChange state currentDate
                    ]
                ]

        highlighted =
            [ Styles.highlightStyle, hover [ Styles.highlightStyle ] ]

        amPmPicker config =
            div [ css [ padding2 (px 40) (px 0) ] ]
                [ div
                    [ onMouseDownPreventDefault <| amPmPickerHandler pickerType config stateValue currentDate "AM"
                    , onTouchStartPreventDefault <| amPmPickerHandler pickerType config stateValue currentDate "AM"
                    , css [ Styles.amPmStyle ]
                    , case stateValue.time.amPm of
                        Just "AM" ->
                            css [ highlighted ]

                        _ ->
                            css []
                    ]
                    [ text "AM" ]
                , div
                    [ onMouseDownPreventDefault <| amPmPickerHandler pickerType config stateValue currentDate "PM"
                    , onTouchStartPreventDefault <| amPmPickerHandler pickerType config stateValue currentDate "PM"
                    , css [ Styles.amPmStyle ]
                    , case stateValue.time.amPm of
                        Just "PM" ->
                            css [ highlighted ]

                        _ ->
                            css []
                    ]
                    [ text "PM" ]
                ]
    in
    case pickerType of
        DateType _ ->
            text ""

        DateTimeType config ->
            html config

        TimeType config ->
            html config


calendar : Type msg -> State -> Maybe Date -> Html msg
calendar pickerType state currentDate =
    let
        stateValue =
            getStateValue state

        html : Config (DatePickerConfig a) msg -> Html msg
        html config =
            case stateValue.titleDate of
                Nothing ->
                    Html.text ""

                Just titleDate ->
                    let
                        firstDay =
                            Date.Extra.Core.toFirstOfMonth titleDate
                                |> Date.dayOfWeek
                                |> DateTimePicker.DateUtils.dayToInt config.firstDayOfWeek

                        month =
                            Date.month titleDate

                        year =
                            Date.year titleDate

                        days =
                            DateTimePicker.DateUtils.generateCalendar config.firstDayOfWeek month year

                        header =
                            thead []
                                [ tr
                                    []
                                    (dayNames config)
                                ]

                        matchesDay : Maybe Date -> DateTimePicker.DateUtils.Day -> Bool
                        matchesDay reference day =
                            let
                                date =
                                    DateTimePicker.DateUtils.toDate year month day
                            in
                            reference
                                |> Maybe.map
                                    (\current ->
                                        (Date.day date == Date.day current)
                                            && (Date.month date == Date.month current)
                                            && (Date.year date == Date.year current)
                                    )
                                |> Maybe.withDefault False

                        isInRange day =
                            let
                                date =
                                    DateTimePicker.DateUtils.toDate year month day
                            in
                            case config.earliestDate of
                                Nothing ->
                                    True

                                Just earliestDate ->
                                    -- we allow the date containing the earliestDate to be selected
                                    Date.toTime date >= (Date.toTime earliestDate - 24 * Time.hour)

                        toCell day =
                            let
                                selectedDate =
                                    DateTimePicker.DateUtils.toDate year month day

                                styles =
                                    List.concat
                                        [ case day.monthType of
                                            DateTimePicker.DateUtils.Previous ->
                                                [ color Styles.fadeText ]

                                            DateTimePicker.DateUtils.Current ->
                                                []

                                            DateTimePicker.DateUtils.Next ->
                                                [ color Styles.fadeText ]
                                        , if isInRange day then
                                            []
                                          else
                                            [ backgroundColor inherit
                                            , cursor default
                                            , color Styles.darkGray
                                            , hover
                                                [ backgroundColor inherit
                                                ]
                                            ]
                                        , if matchesDay stateValue.date day then
                                            [ Styles.highlightStyle
                                            , hover [ Styles.highlightStyle ]
                                            ]
                                          else if matchesDay stateValue.today day then
                                            [ property "box-shadow" "inset 0 0 7px 0 #76abd9"
                                            , Styles.highlightBorderStyle
                                            , hover
                                                [ backgroundColor Styles.highlightSelectedDay ]
                                            ]
                                          else
                                            []
                                        ]

                                handler =
                                    dateClickHandler pickerType stateValue year month day

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
                                      ]
                                    , handlers
                                    ]
                                )
                                [ text <| toString day.day ]

                        toWeekRow week =
                            tr [] (List.map toCell week)

                        body =
                            tbody [] (List.map toWeekRow days)
                    in
                    Html.table
                        [ css
                            [ backgroundColor (hex "#ffffff")
                            , Styles.tableStyle
                            , width auto
                            , margin (px 0)
                            , descendants
                                [ thead
                                    []
                                , td
                                    [ Styles.dayStyle
                                    , hover
                                        [ backgroundColor Styles.highlightedDay
                                        , Styles.highlightBorderStyle
                                        ]
                                    ]
                                , th
                                    [ Styles.dayStyle
                                    , backgroundColor Styles.lightGray
                                    , fontWeight normal
                                    , borderBottom3 (px 1) solid Styles.darkGray
                                    ]
                                ]
                            ]
                        ]
                        [ header
                        , body
                        ]
    in
    case pickerType of
        DateType config ->
            html config

        DateTimeType config ->
            html config

        TimeType config ->
            text ""


dayNames : Config (DatePickerConfig a) msg -> List (Html msg)
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
            DateTimePicker.DateUtils.dayToInt Date.Sun config.firstDayOfWeek
    in
    rotate shiftAmount days


rotate : Int -> List a -> List a
rotate n xs =
    List.drop n xs ++ List.take n xs



-- EVENT HANDLERS


inputChangeHandler : Config a msg -> StateValue -> Maybe Date -> Maybe Date -> msg
inputChangeHandler config stateValue currentDate maybeDate =
    case maybeDate of
        Just date ->
            let
                updateTime time =
                    { time
                        | hour = Date.hour date |> DateTimePicker.DateUtils.fromMillitaryHour |> Just
                        , minute = Just (Date.minute date)
                        , amPm = Date.hour date |> DateTimePicker.DateUtils.fromMillitaryAmPm |> Just
                    }

                updatedValue =
                    { stateValue
                        | date = Just date
                        , time = updateTime stateValue.time
                        , inputFocused = False
                        , event = "inputChangeHandler"
                    }
            in
            config.onChange (InternalState updatedValue) maybeDate

        Nothing ->
            let
                ( updatedTime, updatedActiveTimeIndicator, updatedDate ) =
                    case currentDate of
                        Just _ ->
                            ( { hour = Nothing, minute = Nothing, amPm = Nothing }
                            , Just DateTimePicker.Internal.HourIndicator
                            , Nothing
                            )

                        Nothing ->
                            ( stateValue.time
                            , stateValue.activeTimeIndicator
                            , stateValue.date
                            )

                updatedValue =
                    { stateValue
                        | date = updatedDate
                        , time = updatedTime
                        , hourPickerStart = initialStateValue.hourPickerStart
                        , minutePickerStart = initialStateValue.minutePickerStart
                        , inputFocused = False
                        , event = "inputChangeHandler"
                        , activeTimeIndicator = updatedActiveTimeIndicator
                    }
            in
            config.onChange (InternalState updatedValue) maybeDate


hourClickHandler : Type msg -> StateValue -> Int -> msg
hourClickHandler pickerType stateValue hour =
    let
        time =
            stateValue.time

        updatedStateValue =
            { stateValue | time = { time | hour = Just hour }, event = "hourClickHandler" }

        ( updatedDate, forceCloseWithDate ) =
            case ( stateValue.time.minute, stateValue.time.amPm, stateValue.date ) of
                ( Just minute, Just amPm, Just date ) ->
                    ( Just <| DateTimePicker.DateUtils.setTime date hour minute amPm
                    , True
                    )

                _ ->
                    ( Nothing, False )

        ( updatedTime, forceCloseTimeOnly ) =
            case ( updatedStateValue.time.minute, updatedStateValue.time.amPm ) of
                ( Just minute, Just amPm ) ->
                    ( Just <| DateTimePicker.DateUtils.toTime hour minute amPm
                    , True
                    )

                _ ->
                    ( Nothing, False )

        withDateHandler config =
            config.onChange (InternalState { updatedStateValue | forceClose = forceCloseWithDate }) updatedDate

        justTimeHandler config =
            config.onChange (InternalState { updatedStateValue | forceClose = forceCloseTimeOnly }) updatedTime
    in
    case pickerType of
        DateType config ->
            withDateHandler config

        DateTimeType config ->
            withDateHandler config

        TimeType config ->
            justTimeHandler config


minuteClickHandler : Type msg -> StateValue -> Int -> msg
minuteClickHandler pickerType stateValue minute =
    let
        time =
            stateValue.time

        updatedStateValue =
            { stateValue | time = { time | minute = Just minute }, event = "minuteClickHandler" }

        ( updatedDate, forceCloseWithDate ) =
            case ( stateValue.time.hour, stateValue.time.amPm, stateValue.date ) of
                ( Just hour, Just amPm, Just date ) ->
                    ( Just <| DateTimePicker.DateUtils.setTime date hour minute amPm
                    , True
                    )

                _ ->
                    ( Nothing, False )

        ( updatedTime, forceCloseTimeOnly ) =
            case ( updatedStateValue.time.hour, updatedStateValue.time.amPm ) of
                ( Just hour, Just amPm ) ->
                    ( Just <| DateTimePicker.DateUtils.toTime hour minute amPm
                    , True
                    )

                _ ->
                    ( Nothing, False )

        withDateHandler config =
            config.onChange (InternalState { updatedStateValue | forceClose = forceCloseWithDate }) updatedDate

        justTimeHandler config =
            config.onChange (InternalState { updatedStateValue | forceClose = forceCloseTimeOnly }) updatedTime
    in
    case pickerType of
        DateType config ->
            withDateHandler config

        DateTimeType config ->
            withDateHandler config

        TimeType config ->
            justTimeHandler config


amPmClickHandler : Type msg -> StateValue -> String -> msg
amPmClickHandler pickerType stateValue amPm =
    let
        time =
            stateValue.time

        updatedStateValue =
            { stateValue
                | time =
                    { time
                        | amPm =
                            if String.isEmpty amPm then
                                Nothing
                            else
                                Just amPm
                    }
                , event = "amPmClickHandler"
            }

        ( updatedDate, forceCloseWithDate ) =
            case ( stateValue.time.hour, stateValue.time.minute, stateValue.date ) of
                ( Just hour, Just minute, Just date ) ->
                    ( Just <| DateTimePicker.DateUtils.setTime date hour minute amPm
                    , True
                    )

                _ ->
                    ( Nothing, False )

        ( updatedTime, forceCloseTimeOnly ) =
            case ( updatedStateValue.time.hour, updatedStateValue.time.minute ) of
                ( Just hour, Just minute ) ->
                    ( Just <| DateTimePicker.DateUtils.toTime hour minute amPm
                    , True
                    )

                _ ->
                    ( Nothing, False )

        withDateHandler config =
            config.onChange (InternalState { updatedStateValue | forceClose = forceCloseWithDate }) updatedDate

        justTimeHandler config =
            config.onChange (InternalState { updatedStateValue | forceClose = forceCloseTimeOnly }) updatedTime
    in
    case pickerType of
        DateType config ->
            withDateHandler config

        DateTimeType config ->
            withDateHandler config

        TimeType config ->
            justTimeHandler config


dateClickHandler : Type msg -> StateValue -> Int -> Date.Month -> DateTimePicker.DateUtils.Day -> msg
dateClickHandler pickerType stateValue year month day =
    let
        selectedDate =
            DateTimePicker.DateUtils.toDate year month day

        updatedStateValue =
            { stateValue
                | date = Just <| selectedDate
                , forceClose = forceClose
                , activeTimeIndicator =
                    if stateValue.time.hour == Nothing then
                        Just DateTimePicker.Internal.HourIndicator
                    else if stateValue.time.minute == Nothing then
                        Just DateTimePicker.Internal.MinuteIndicator
                    else if stateValue.time.amPm == Nothing then
                        Just DateTimePicker.Internal.AMPMIndicator
                    else
                        Nothing
            }

        ( updatedDate, forceClose ) =
            case ( pickerType, stateValue.time.hour, stateValue.time.minute, stateValue.time.amPm ) of
                ( DateTimeType _, Just hour, Just minute, Just amPm ) ->
                    ( Just <| DateTimePicker.DateUtils.setTime selectedDate hour minute amPm
                    , True
                    )

                ( DateType _, _, _, _ ) ->
                    ( Just selectedDate
                    , True
                    )

                _ ->
                    ( Nothing, False )

        handler config =
            case day.monthType of
                DateTimePicker.DateUtils.Previous ->
                    gotoPreviousMonth config (InternalState updatedStateValue) updatedDate

                DateTimePicker.DateUtils.Next ->
                    gotoNextMonth config (InternalState updatedStateValue) updatedDate

                DateTimePicker.DateUtils.Current ->
                    config.onChange (InternalState updatedStateValue) updatedDate
    in
    case pickerType of
        DateType config ->
            handler config

        DateTimeType config ->
            handler config

        TimeType config ->
            handler config


datePickerFocused : Type msg -> Config a msg -> StateValue -> Maybe Date -> msg
datePickerFocused pickerType config stateValue currentDate =
    let
        updatedTitleDate =
            case currentDate of
                Nothing ->
                    stateValue.titleDate

                Just _ ->
                    currentDate

        updateTime time =
            { time
                | hour = currentDate |> Maybe.map (Date.hour >> DateTimePicker.DateUtils.fromMillitaryHour)
                , minute = currentDate |> Maybe.map Date.minute
                , amPm = currentDate |> Maybe.map (Date.hour >> DateTimePicker.DateUtils.fromMillitaryAmPm)
            }
    in
    config.onChange
        (InternalState
            { stateValue
                | inputFocused = True
                , event = "onFocus"
                , titleDate = updatedTitleDate
                , date = currentDate
                , forceClose = False
                , time = updateTime stateValue.time
                , activeTimeIndicator =
                    case pickerType of
                        TimeType _ ->
                            Just DateTimePicker.Internal.HourIndicator

                        _ ->
                            Nothing
            }
        )
        currentDate


onChangeHandler : Type msg -> StateValue -> Maybe Date -> msg
onChangeHandler pickerType stateValue currentDate =
    let
        justDateHandler config =
            config.onChange (InternalState stateValue) stateValue.date

        withTimeHandler config =
            case ( stateValue.date, stateValue.time.hour, stateValue.time.minute, stateValue.time.amPm ) of
                ( Just date, Just hour, Just minute, Just amPm ) ->
                    config.onChange (InternalState stateValue) <| Just <| DateTimePicker.DateUtils.setTime date hour minute amPm

                _ ->
                    config.onChange (InternalState stateValue) Nothing
    in
    case pickerType of
        DateType config ->
            justDateHandler config

        DateTimeType config ->
            withTimeHandler config

        TimeType config ->
            withTimeHandler config


hourUpHandler : Config config msg -> StateValue -> Maybe Date -> msg
hourUpHandler config stateValue currentDate =
    let
        updatedState =
            if stateValue.hourPickerStart - 6 >= 1 then
                { stateValue | hourPickerStart = stateValue.hourPickerStart - 6 }
            else
                stateValue
    in
    config.onChange (InternalState updatedState) currentDate


hourDownHandler : Config config msg -> StateValue -> Maybe Date -> msg
hourDownHandler config stateValue currentDate =
    let
        updatedState =
            if stateValue.hourPickerStart + 6 <= 12 then
                { stateValue | hourPickerStart = stateValue.hourPickerStart + 6 }
            else
                stateValue
    in
    config.onChange (InternalState updatedState) currentDate


minuteUpHandler : Config config msg -> StateValue -> Maybe Date -> msg
minuteUpHandler config stateValue currentDate =
    let
        updatedState =
            if stateValue.minutePickerStart - 6 >= 0 then
                { stateValue | minutePickerStart = stateValue.minutePickerStart - 6 }
            else
                stateValue
    in
    config.onChange (InternalState updatedState) currentDate


minuteDownHandler : Config config msg -> StateValue -> Maybe Date -> msg
minuteDownHandler config stateValue currentDate =
    let
        updatedState =
            if stateValue.minutePickerStart + 6 <= 59 then
                { stateValue | minutePickerStart = stateValue.minutePickerStart + 6 }
            else
                stateValue
    in
    config.onChange (InternalState updatedState) currentDate


timeIndicatorHandler : Config config msg -> StateValue -> Maybe Date -> DateTimePicker.Internal.TimeIndicator -> msg
timeIndicatorHandler config stateValue currentDate timeIndicator =
    let
        updatedState =
            { stateValue
                | activeTimeIndicator = updatedActiveTimeIndicator
                , currentAngle = currentAngle
            }

        updatedActiveTimeIndicator =
            if stateValue.activeTimeIndicator == Just timeIndicator then
                Nothing
            else
                Just timeIndicator

        currentAngle =
            case ( timeIndicator, stateValue.time.hour, stateValue.time.minute ) of
                ( DateTimePicker.Internal.HourIndicator, Just hour, _ ) ->
                    DateTimePicker.ClockUtils.hourToAngle hour

                ( DateTimePicker.Internal.MinuteIndicator, _, Just minute ) ->
                    DateTimePicker.ClockUtils.minuteToAngle minute

                ( _, _, _ ) ->
                    Nothing
    in
    config.onChange (InternalState updatedState) currentDate


amPmIndicatorHandler : Config config msg -> StateValue -> Maybe Date -> msg
amPmIndicatorHandler config stateValue currentDate =
    let
        updateTime time =
            case time.amPm of
                Just "AM" ->
                    { time | amPm = Just "PM" }

                Just "PM" ->
                    { time | amPm = Just "AM" }

                _ ->
                    { time | amPm = Just "AM" }

        updatedState =
            { stateValue
                | activeTimeIndicator = Just DateTimePicker.Internal.AMPMIndicator
                , time = updateTime stateValue.time
            }
    in
    config.onChange (InternalState updatedState) currentDate


amPmPickerHandler : Type msg -> Config config msg -> StateValue -> Maybe Date -> String -> msg
amPmPickerHandler pickerType config stateValue currentDate amPm =
    let
        time =
            stateValue.time

        updatedTime =
            { time | amPm = Just amPm }

        updatedState =
            { stateValue | time = updatedTime }
                |> updateTimeIndicator
    in
    config.onChange
        (InternalState updatedState)
        (updateCurrentDate pickerType updatedState)


datePickerDialogCss : List Snippet
datePickerDialogCss =
    [ css
        [ Styles.borderBoxStyle
        , Styles.headerStyle
        , position relative
        ]
    ]
