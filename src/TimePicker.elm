module TimePicker exposing
    ( init, Model, Time
    , view
    )

{-|

@docs init, Model, Time
@docs view

-}

import Css exposing (..)
import Css.Global exposing (descendants)
import DateTimePicker.Config exposing (TimePickerConfig)
import DateTimePicker.DateTime as DateTime
import DateTimePicker.DateUtils
import DateTimePicker.Events exposing (onMouseDownPreventDefault, onTouchStartPreventDefault)
import DateTimePicker.Formatter
import DateTimePicker.Internal exposing (InternalState(..), StateValue, getStateValue, initialStateValue, initialStateValueWithToday)
import DateTimePicker.Styles as Styles
import DateTimePicker.Svg
import Html.Styled as Html exposing (Html, div, tbody, td, text, tfoot, thead, tr)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.TextInput.V7 as TextInput
import String
import Time


{-| The state of the time picker.
-}
type alias Model =
    InternalState


{-| -}
type alias Time =
    DateTime.DateTime


type alias TimeSelection =
    { hour : Maybe Int
    , minute : Maybe Int
    , amPm : Maybe String
    }


{-| Pass in the current time.
-}
init : Time -> Model
init today =
    InternalState (initialStateValueWithToday today)


{-| -}
view : String -> TimePickerConfig msg -> List (TextInput.Attribute String msg) -> Model -> Maybe DateTime.DateTime -> Html msg
view label config attributes ((InternalState stateValue) as state) currentDate =
    Html.node "time-picker"
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
            Html.node "time-picker-dialog"
                [ onMouseDownPreventDefault (config.onChange state currentDate)
                , css [ display block, Styles.dialog ]
                ]
                [ timePickerDialog config state currentDate ]

          else
            Html.text ""
        ]


timePickerDialog : TimePickerConfig msg -> Model -> Maybe DateTime.DateTime -> Html msg
timePickerDialog ({ fromInput } as config) state currentDate =
    let
        stateValue =
            getStateValue state

        time =
            timeFromTextInputString fromInput stateValue.textInputValue

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
            let
                hourClickHandler =
                    cellClickHandler config
                        stateValue
                        { time | hour = Just hour }
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
                    cellClickHandler config
                        stateValue
                        { time | minute = Just minute }
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
                    cellClickHandler config
                        stateValue
                        { time | amPm = Just ampm }

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

        upArrows =
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

        downArrows =
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
    in
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
                [ thead [] upArrows
                , tbody [] timeSelector
                , tfoot [] downArrows
                ]
            ]
        ]



-- ACTIONS


hourUpHandler :
    { config
        | onChange : Model -> Maybe DateTime.DateTime -> msg
        , toInput : DateTime.DateTime -> String
    }
    -> StateValue
    -> Maybe DateTime.DateTime
    -> msg
hourUpHandler config stateValue currentDate =
    let
        updatedState =
            if stateValue.hourPickerStart - 6 >= 1 then
                { stateValue | hourPickerStart = stateValue.hourPickerStart - 6 }

            else
                stateValue
    in
    config.onChange (updateTextInputFromDate config updatedState) currentDate


hourDownHandler :
    { config
        | onChange : Model -> Maybe DateTime.DateTime -> msg
        , toInput : DateTime.DateTime -> String
    }
    -> StateValue
    -> Maybe DateTime.DateTime
    -> msg
hourDownHandler config stateValue currentDate =
    let
        updatedState =
            if stateValue.hourPickerStart + 6 <= 12 then
                { stateValue | hourPickerStart = stateValue.hourPickerStart + 6 }

            else
                stateValue
    in
    config.onChange (updateTextInputFromDate config updatedState) currentDate


minuteUpHandler :
    { config
        | onChange : Model -> Maybe DateTime.DateTime -> msg
        , toInput : DateTime.DateTime -> String
    }
    -> StateValue
    -> Maybe DateTime.DateTime
    -> msg
minuteUpHandler config stateValue currentDate =
    let
        updatedState =
            if stateValue.minutePickerStart - 6 >= 0 then
                { stateValue | minutePickerStart = stateValue.minutePickerStart - 6 }

            else
                stateValue
    in
    config.onChange (updateTextInputFromDate config updatedState) currentDate


minuteDownHandler :
    { config
        | onChange : Model -> Maybe DateTime.DateTime -> msg
        , toInput : DateTime.DateTime -> String
    }
    -> StateValue
    -> Maybe DateTime.DateTime
    -> msg
minuteDownHandler config stateValue currentDate =
    let
        updatedState =
            if stateValue.minutePickerStart + 6 <= 59 then
                { stateValue | minutePickerStart = stateValue.minutePickerStart + 6 }

            else
                stateValue
    in
    config.onChange (updateTextInputFromDate config updatedState) currentDate



-- Cell click handling


cellClickHandler :
    { config
        | onChange : Model -> Maybe DateTime.DateTime -> msg
        , toInput : DateTime.DateTime -> String
    }
    -> StateValue
    -> TimeSelection
    -> msg
cellClickHandler config stateValue timeSelection =
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

        adjustedSelectedDate =
            case stateValue.date of
                Just currentDate ->
                    currentDate
                        |> setHour
                        |> setMinute

                Nothing ->
                    DateTime.fromDate 0 Time.Jan 1
                        |> setHour
                        |> setMinute
    in
    config.onChange (updateTextInputFromDate config { stateValue | date = Just adjustedSelectedDate })
        (Just adjustedSelectedDate)



-- Text parsing of time


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



-- Misc


blurInputHandler :
    { config
        | onChange : Model -> Maybe DateTime.DateTime -> msg
        , fromInput : String -> Maybe DateTime.DateTime
        , toInput : DateTime.DateTime -> String
    }
    -> StateValue
    -> Maybe DateTime.DateTime
    -> msg
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


datePickerFocused :
    { config
        | onChange : Model -> Maybe DateTime.DateTime -> msg
        , toInput : DateTime.DateTime -> String
    }
    -> StateValue
    -> Maybe DateTime.DateTime
    -> msg
datePickerFocused config stateValue currentDate =
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


setTextInput : String -> StateValue -> Model
setTextInput value state =
    InternalState { state | textInputValue = value }


updateTextInputFromDate :
    { config | toInput : DateTime.DateTime -> String }
    -> StateValue
    -> Model
updateTextInputFromDate config state =
    InternalState
        { state
            | textInputValue =
                Maybe.map config.toInput state.date
                    |> Maybe.withDefault state.textInputValue
        }
