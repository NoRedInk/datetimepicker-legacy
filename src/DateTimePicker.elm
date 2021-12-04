module DateTimePicker exposing
    ( DateTime, dateTime
    , datePickerWithConfig, dateTimePickerWithConfig, timePickerWithConfig
    , initialStateWithToday
    , State
    )

{-|


# This is a heavily-modified version of <https://github.com/abadi199/datetimepicker/blob/master/src/DateTimePicker/Svg.elm>

@docs DateTime, dateTime


# View

@docs datePickerWithConfig, dateTimePickerWithConfig, timePickerWithConfig


# Initial

@docs initialStateWithToday


# Internal State

@docs State

-}

import Css exposing (..)
import Css.Global exposing (descendants)
import DateTimePicker.Config exposing (Config, DatePickerConfig, Type(..))
import DateTimePicker.DateTime as DateTime
import DateTimePicker.DateUtils
import DateTimePicker.Events exposing (onMouseDownPreventDefault, onTouchStartPreventDefault)
import DateTimePicker.Formatter exposing (accessibilityDateFormatter)
import DateTimePicker.Internal exposing (DateSelection, InternalState(..), StateValue, TimeSelection, getStateValue, initialStateValue, initialStateValueWithToday)
import DateTimePicker.Styles as Styles
import DateTimePicker.Svg
import Html.Styled as Html exposing (Html, div, span, tbody, td, text, tfoot, th, thead, tr)
import Html.Styled.Attributes exposing (attribute, css)
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.TextInput.V7 as TextInput
import String
import Time



-- MODEL


{-| The state of the date time picker (for Internal Use)
-}
type alias State =
    InternalState


{-| The representation of a date and time in the picker
-}
type alias DateTime =
    DateTime.DateTime


{-| Construct a DateTime
-}
dateTime : Int -> Time.Month -> Int -> Int -> Int -> DateTime.DateTime
dateTime =
    DateTime.DateTime


{-| Initial state of the DatePicker with today Date
-}
initialStateWithToday : DateTime.DateTime -> State
initialStateWithToday today =
    InternalState
        (initialStateValueWithToday today)



-- ACTIONS


gotoNextMonth : Config a msg -> State -> (Maybe DateTime.DateTime -> msg)
gotoNextMonth config state =
    let
        stateValue =
            getStateValue state

        updatedTitleDate =
            Maybe.map (DateTime.addMonths 1) stateValue.titleDate
    in
    config.onChange <| InternalState { stateValue | titleDate = updatedTitleDate }


gotoNextYear : Config a msg -> State -> (Maybe DateTime.DateTime -> msg)
gotoNextYear config state =
    let
        stateValue =
            getStateValue state

        updatedTitleDate =
            Maybe.map (DateTime.addMonths 12) stateValue.titleDate
    in
    config.onChange <| InternalState { stateValue | titleDate = updatedTitleDate }


gotoPreviousMonth : Config a msg -> State -> (Maybe DateTime.DateTime -> msg)
gotoPreviousMonth config state =
    let
        stateValue =
            getStateValue state

        updatedTitleDate =
            Maybe.map (DateTime.addMonths -1) stateValue.titleDate
    in
    config.onChange <| InternalState { stateValue | titleDate = updatedTitleDate }


gotoPreviousYear : Config a msg -> State -> (Maybe DateTime.DateTime -> msg)
gotoPreviousYear config state =
    let
        stateValue =
            getStateValue state

        updatedTitleDate =
            Maybe.map (DateTime.addMonths -12) stateValue.titleDate
    in
    config.onChange <| InternalState { stateValue | titleDate = updatedTitleDate }



-- VIEWS


{-| Date Picker view function with custom configuration.

Example:
type alias Model = { datePickerState : DateTimePicker.State, value : Maybe DateTime.DateTime }

    type Msg
        = DatePickerChanged DateTimePicker.State (Maybe DateTime.DateTime)

    customConfig =
        let
            default =
                DateTimePicker.defaultConfig DatePickerChanged
        in
        { default | firstDayOfWeek = Date.Mon }

    view =
        DateTimePicker.datePickerWithConfig "Date and Time Picker"
            customConfig
            DateTimePicker.defaultDatePickerConfig
            [ class "my-datepicker" ]
            model.datePickerState
            model.value

-}
datePickerWithConfig : String -> Config DatePickerConfig msg -> List (TextInput.Attribute String msg) -> State -> Maybe DateTime.DateTime -> Html msg
datePickerWithConfig label config =
    view label (DateType config)


{-| Date and Time Picker view with custom configuration
Example:
type alias Model = { dateTimePickerState : DateTimePicker.State, value : Maybe DateTime.DateTime }

    type Msg
        = DatePickerChanged DateTimePicker.State (Maybe DateTime.DateTime)

    customConfig =
        let
            default =
                DateTimePicker.defaultDateTimePickerConfig DatePickerChanged
        in
        { default | firstDayOfWeek = Date.Mon }

    view =
        DateTimePicker.dateTimePickerWithConfig "Date and Time Picker"
            customConfig
            [ class "my-datetimepicker" ]
            model.dateTimePickerState
            model.value

-}
dateTimePickerWithConfig : String -> Config DatePickerConfig msg -> List (TextInput.Attribute String msg) -> State -> Maybe DateTime.DateTime -> Html msg
dateTimePickerWithConfig label config =
    view label (DateTimeType config)


{-| Time Picker view with custom configuration
Example:
type alias Model = { timePickerState : DateTimePicker.State, value : Maybe DateTime.DateTime }

    type Msg
        = TimePickerChanged DateTimePicker.State (Maybe DateTime.DateTime)

    customConfig =
        DateTimePicker.defaultTimePickerConfig TimePickerChanged

    view =
        DateTimePicker.timePickerWithConfig "Time picker"
            customConfig
            [ class "my-datetimepicker" ]
            model.timePickerState
            model.value

-}
timePickerWithConfig : String -> Config {} msg -> List (TextInput.Attribute String msg) -> State -> Maybe DateTime.DateTime -> Html msg
timePickerWithConfig label config =
    view label (TimeType config)


view : String -> Type msg -> List (TextInput.Attribute String msg) -> State -> Maybe DateTime.DateTime -> Html msg
view label pickerType attributes state currentDate =
    let
        stateValue =
            getStateValue state

        html config =
            Html.node "date-time-picker"
                (css [ position relative ] :: config.attributes)
                [ viewInput label pickerType attributes config stateValue currentDate
                , if config.usePicker && stateValue.inputFocused then
                    dialog pickerType state currentDate

                  else
                    Html.text ""
                ]
    in
    case pickerType of
        DateType config ->
            html config

        DateTimeType config ->
            html config

        TimeType config ->
            html config


viewInput : String -> Type msg -> List (TextInput.Attribute String msg) -> Config a msg -> StateValue -> Maybe DateTime.DateTime -> Html msg
viewInput label pickerType attributes config stateValue currentDate =
    TextInput.view label
        ([ TextInput.onFocus (datePickerFocused pickerType config stateValue currentDate)
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



-- VIEW HELPERS


dialog : Type msg -> State -> Maybe DateTime.DateTime -> Html msg
dialog pickerType state currentDate =
    let
        attributes config =
            [ onMouseDownPreventDefault <| config.onChange state currentDate
            , css
                [ fontFamilies [ "Arial", "Helvetica", "sans-serif" ]
                , fontSize (px 14)
                , boxSizing borderBox
                , position absolute
                , border3 (px 1) solid Colors.gray85
                , property "z-index" "1"
                , displayFlex
                ]
            ]

        withTimeAttributes config =
            attributes config
    in
    case pickerType of
        DateType datePickerConfig ->
            dialogNode (attributes datePickerConfig)
                [ datePickerDialog pickerType state currentDate ]

        TimeType timePickerConfig ->
            dialogNode (withTimeAttributes timePickerConfig)
                [ timePickerDialog pickerType state currentDate ]

        DateTimeType timePickerConfig ->
            dialogNode (withTimeAttributes timePickerConfig)
                [ datePickerDialog pickerType state currentDate
                , timePickerDialog pickerType state currentDate
                ]


dialogNode : List (Html.Attribute msg) -> List (Html msg) -> Html msg
dialogNode attributes =
    Html.node "date-time-picker-dialog"
        ([ css [ display block ] ]
            ++ attributes
        )


datePickerDialog : Type msg -> State -> Maybe DateTime.DateTime -> Html msg
datePickerDialog pickerType state currentDate =
    let
        stateValue =
            getStateValue state

        html config =
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
                , calendar pickerType state
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
    in
    case pickerType of
        DateType config ->
            html config

        DateTimeType config ->
            html config

        TimeType _ ->
            text ""


navigation : Config DatePickerConfig msg -> State -> Maybe DateTime.DateTime -> List (Html msg)
navigation config state currentDate =
    [ previousYearButton config state currentDate
    , previousButton config state currentDate
    , title state
    , nextButton config state currentDate
    , nextYearButton config state currentDate
    ]


title : State -> Html msg
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


previousYearButton : Config DatePickerConfig msg -> State -> Maybe DateTime.DateTime -> Html msg
previousYearButton config state currentDate =
    if config.allowYearNavigation then
        ClickableSvg.button "Previous Year"
            DateTimePicker.Svg.doubleLeftArrow
            [ ClickableSvg.onClick (gotoPreviousYear config state currentDate)
            ]

    else
        Html.text ""


previousButton : Config DatePickerConfig msg -> State -> Maybe DateTime.DateTime -> Html msg
previousButton config state currentDate =
    ClickableSvg.button "Previous month"
        DateTimePicker.Svg.leftArrow
        [ ClickableSvg.onClick (gotoPreviousMonth config state currentDate)
        ]


nextButton : Config DatePickerConfig msg -> State -> Maybe DateTime.DateTime -> Html msg
nextButton config state currentDate =
    ClickableSvg.button "Next month"
        DateTimePicker.Svg.rightArrow
        [ ClickableSvg.onClick (gotoNextMonth config state currentDate)
        ]


nextYearButton : Config DatePickerConfig msg -> State -> Maybe DateTime.DateTime -> Html msg
nextYearButton config state currentDate =
    if config.allowYearNavigation then
        ClickableSvg.button "Next Year"
            DateTimePicker.Svg.doubleRightArrow
            [ ClickableSvg.onClick (gotoNextYear config state currentDate)
            ]

    else
        Html.text ""


timePickerDialog : Type msg -> State -> Maybe DateTime.DateTime -> Html msg
timePickerDialog pickerType state currentDate =
    let
        stateValue =
            getStateValue state
    in
    case pickerType of
        DateType _ ->
            text ""

        DateTimeType { fromInput } ->
            digitalTimePickerDialog pickerType
                state
                currentDate
                (timeFromTextInputString fromInput stateValue.textInputValue)

        TimeType { fromInput } ->
            digitalTimePickerDialog pickerType
                state
                currentDate
                (timeFromTextInputString fromInput stateValue.textInputValue)


digitalTimePickerDialog : Type msg -> State -> Maybe DateTime.DateTime -> TimeSelection -> Html msg
digitalTimePickerDialog pickerType state currentDate time =
    let
        stateValue =
            getStateValue state

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

        timeCellClick =
            cellClickHandler pickerType stateValue Nothing

        hourCell hour =
            let
                hourClickHandler =
                    timeCellClick { time | hour = Just hour }
            in
            td
                [ onMouseDownPreventDefault hourClickHandler
                , onTouchStartPreventDefault hourClickHandler
                , case time.hour of
                    Just stateHour ->
                        if stateHour == hour then
                            css [ Styles.highlightStyle ]

                        else
                            css []

                    Nothing ->
                        css []
                ]
                [ text <| (String.fromInt >> DateTimePicker.DateUtils.padding) hour ]

        minuteCell minute =
            let
                minuteClickHandler =
                    timeCellClick { time | minute = Just minute }
            in
            td
                [ onMouseDownPreventDefault minuteClickHandler
                , onTouchStartPreventDefault minuteClickHandler
                , case time.minute of
                    Just stateMinute ->
                        if stateMinute == minute then
                            css [ Styles.highlightStyle ]

                        else
                            css []

                    Nothing ->
                        css []
                ]
                [ text <| (String.fromInt >> DateTimePicker.DateUtils.padding) minute ]

        amPmCell ampm =
            let
                defaultStyles =
                    if String.isEmpty ampm then
                        css [ Styles.emptyCellStyle ]

                    else
                        css []

                styles =
                    case time.amPm of
                        Just stateAmPm ->
                            if stateAmPm == ampm then
                                css [ Styles.highlightStyle ]

                            else
                                defaultStyles

                        Nothing ->
                            defaultStyles

                amPmClickHandler =
                    timeCellClick { time | amPm = Just ampm }

                handlers =
                    if String.isEmpty ampm then
                        []

                    else
                        [ onMouseDownPreventDefault amPmClickHandler
                        , onTouchStartPreventDefault amPmClickHandler
                        ]
            in
            td (styles :: handlers) [ text ampm ]

        upArrowTd =
            Html.styled td
                [ borderBottom3 (px 1) solid Colors.gray85
                , height (Css.px 18)
                ]
                []

        upArrows config =
            [ tr [ css [ backgroundColor Colors.gray96 ] ]
                [ upArrowTd
                    [ ClickableSvg.button "Earlier hours"
                        DateTimePicker.Svg.upArrow
                        [ ClickableSvg.onClick (hourUpHandler config stateValue currentDate)
                        , ClickableSvg.exactHeight 24
                        ]
                    ]
                , upArrowTd
                    [ ClickableSvg.button "Earlier minutes"
                        DateTimePicker.Svg.upArrow
                        [ ClickableSvg.onClick (minuteUpHandler config stateValue currentDate)
                        , ClickableSvg.exactHeight 24
                        ]
                    ]
                , upArrowTd []
                ]
            ]

        downArrowTd =
            Html.styled td
                [ borderTop3 (px 1) solid Colors.gray85
                , height (Css.px 18)
                ]
                []

        downArrows config =
            [ tr [ css [ backgroundColor Colors.gray96 ] ]
                [ downArrowTd
                    [ ClickableSvg.button "Later hours"
                        DateTimePicker.Svg.downArrow
                        [ ClickableSvg.onClick (hourDownHandler config stateValue currentDate)
                        , ClickableSvg.exactHeight 24
                        ]
                    ]
                , downArrowTd
                    [ ClickableSvg.button "Later minutes"
                        DateTimePicker.Svg.downArrow
                        [ ClickableSvg.onClick (minuteDownHandler config stateValue currentDate)
                        , ClickableSvg.exactHeight 24
                        ]
                    ]
                , downArrowTd []
                ]
            ]

        html config =
            div [ css [ Styles.timePickerDialog ] ]
                [ div
                    [ css
                        [ padding2 (px 10) (px 7)
                        , displayFlex
                        , justifyContent center
                        , alignItems center
                        , backgroundColor Colors.gray96
                        , height (Css.px 37)
                        ]
                    ]
                    [ Maybe.map DateTimePicker.Formatter.timeFormatter currentDate
                        |> Maybe.withDefault "-- : --"
                        |> text
                    ]
                , div
                    [ css
                        [ backgroundColor Colors.white
                        , descendants
                            [ Css.Global.table
                                [ Styles.tableStyle
                                , width (px 120)
                                , descendants [ Css.Global.tr [ verticalAlign top ] ]
                                , descendants
                                    [ Css.Global.tbody
                                        [ descendants
                                            [ Css.Global.td
                                                [ width (pct 33)
                                                , Styles.cellStyle
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                    [ Html.table []
                        [ thead [] (upArrows config)
                        , tbody [] timeSelector
                        , tfoot [] (downArrows config)
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


calendar : Type msg -> State -> Html msg
calendar pickerType state =
    let
        stateValue =
            getStateValue state

        html : Config DatePickerConfig msg -> Html msg
        html config =
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
                                    cellClickHandler pickerType
                                        stateValue
                                        (Just { year = year, month = month, day = day })
                                        { hour = Nothing
                                        , minute = Nothing
                                        , amPm = Nothing
                                        }

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
    in
    case pickerType of
        DateType config ->
            html config

        DateTimeType config ->
            html config

        TimeType _ ->
            text ""


dayNames : Config DatePickerConfig msg -> List (Html msg)
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



-- EVENT HANDLERS


blurInputHandler : Config a msg -> StateValue -> Maybe DateTime.DateTime -> msg
blurInputHandler config stateValue currentDate =
    case config.fromInput stateValue.textInputValue of
        Just date ->
            let
                updatedValue =
                    { stateValue
                        | date = Just date
                        , inputFocused = False
                    }
            in
            config.onChange (updateTextInputFromDate config updatedValue) (Just date)

        Nothing ->
            let
                updatedDate =
                    case currentDate of
                        Just _ ->
                            Nothing

                        Nothing ->
                            stateValue.date

                updatedValue =
                    { stateValue
                        | date = updatedDate
                        , hourPickerStart = initialStateValue.hourPickerStart
                        , minutePickerStart = initialStateValue.minutePickerStart
                        , inputFocused = False
                    }
            in
            config.onChange (setTextInput "" updatedValue) Nothing


cellClickHandler :
    Type msg
    -> StateValue
    -> Maybe DateSelection
    -> TimeSelection
    -> msg
cellClickHandler pickerType stateValue date timeSelection =
    let
        setHour datetime =
            case timeSelection.hour of
                Just hour ->
                    DateTime.setHour hour timeSelection.amPm datetime

                Nothing ->
                    datetime

        setMinute datetime =
            case timeSelection.minute of
                Just minute ->
                    DateTime.setMinute minute datetime

                Nothing ->
                    datetime

        selectedDate =
            case ( date, stateValue.date ) of
                ( Just { year, month, day }, Just currentDate ) ->
                    DateTime.fromParts year
                        month
                        day.day
                        currentDate.hour
                        currentDate.minute

                ( Just { year, month, day }, Nothing ) ->
                    DateTime.fromDate year month day.day
                        |> setHour
                        |> setMinute

                ( Nothing, Just currentDate ) ->
                    currentDate
                        |> setHour
                        |> setMinute

                ( Nothing, Nothing ) ->
                    DateTime.fromDate 0 Time.Jan 1
                        |> setHour
                        |> setMinute

        adjustedSelectedDate =
            case Maybe.map (.day >> .monthType) date of
                Just DateTimePicker.DateUtils.Previous ->
                    DateTime.addMonths -1 selectedDate

                Just DateTimePicker.DateUtils.Next ->
                    DateTime.addMonths 1 selectedDate

                Just DateTimePicker.DateUtils.Current ->
                    selectedDate

                Nothing ->
                    selectedDate

        updatedStateValue =
            { stateValue
                | date = Just <| adjustedSelectedDate
            }

        handler config =
            case Maybe.map (.day >> .monthType) date of
                Just DateTimePicker.DateUtils.Previous ->
                    gotoPreviousMonth config (updateTextInputFromDate config updatedStateValue) (Just adjustedSelectedDate)

                Just DateTimePicker.DateUtils.Next ->
                    gotoNextMonth config (updateTextInputFromDate config updatedStateValue) (Just adjustedSelectedDate)

                Just DateTimePicker.DateUtils.Current ->
                    config.onChange (updateTextInputFromDate config updatedStateValue) (Just adjustedSelectedDate)

                Nothing ->
                    config.onChange (updateTextInputFromDate config updatedStateValue) (Just adjustedSelectedDate)
    in
    case pickerType of
        DateType config ->
            handler config

        DateTimeType config ->
            handler config

        TimeType config ->
            handler config


datePickerFocused : Type msg -> Config a msg -> StateValue -> Maybe DateTime.DateTime -> msg
datePickerFocused pickerType config stateValue currentDate =
    let
        updatedTitleDate =
            case currentDate of
                Nothing ->
                    stateValue.titleDate

                Just _ ->
                    currentDate
    in
    config.onChange
        (updateTextInputFromDate config
            { stateValue
                | inputFocused = True
                , titleDate = updatedTitleDate
                , date = currentDate
            }
        )
        currentDate


hourUpHandler : Config config msg -> StateValue -> Maybe DateTime.DateTime -> msg
hourUpHandler config stateValue currentDate =
    let
        updatedState =
            if stateValue.hourPickerStart - 6 >= 1 then
                { stateValue | hourPickerStart = stateValue.hourPickerStart - 6 }

            else
                stateValue
    in
    config.onChange (updateTextInputFromDate config updatedState) currentDate


hourDownHandler : Config config msg -> StateValue -> Maybe DateTime.DateTime -> msg
hourDownHandler config stateValue currentDate =
    let
        updatedState =
            if stateValue.hourPickerStart + 6 <= 12 then
                { stateValue | hourPickerStart = stateValue.hourPickerStart + 6 }

            else
                stateValue
    in
    config.onChange (updateTextInputFromDate config updatedState) currentDate


minuteUpHandler : Config config msg -> StateValue -> Maybe DateTime.DateTime -> msg
minuteUpHandler config stateValue currentDate =
    let
        updatedState =
            if stateValue.minutePickerStart - 6 >= 0 then
                { stateValue | minutePickerStart = stateValue.minutePickerStart - 6 }

            else
                stateValue
    in
    config.onChange (updateTextInputFromDate config updatedState) currentDate


minuteDownHandler : Config config msg -> StateValue -> Maybe DateTime.DateTime -> msg
minuteDownHandler config stateValue currentDate =
    let
        updatedState =
            if stateValue.minutePickerStart + 6 <= 59 then
                { stateValue | minutePickerStart = stateValue.minutePickerStart + 6 }

            else
                stateValue
    in
    config.onChange (updateTextInputFromDate config updatedState) currentDate


timeFromTextInputString : (String -> Maybe DateTime.DateTime) -> String -> TimeSelection
timeFromTextInputString fromInput textInputValue =
    case fromInput textInputValue of
        Just date ->
            { hour = date.hour |> DateTimePicker.DateUtils.fromMilitaryHour |> Just
            , minute = Just date.minute
            , amPm = date.hour |> DateTimePicker.DateUtils.fromMilitaryAmPm |> Just
            }

        Nothing ->
            { hour = Nothing
            , minute = Nothing
            , amPm = Nothing
            }


setTextInput : String -> StateValue -> State
setTextInput value state =
    InternalState { state | textInputValue = value }


updateTextInputFromDate :
    { config | toInput : DateTime.DateTime -> String }
    -> StateValue
    -> State
updateTextInputFromDate config state =
    InternalState
        { state
            | textInputValue =
                Maybe.map config.toInput state.date
                    |> Maybe.withDefault state.textInputValue
        }
