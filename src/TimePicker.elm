module TimePicker exposing
    ( init, Model, Time
    , view
    , fromString, toString
    )

{-|

@docs init, Model, Time
@docs view
@docs fromString, toString

-}

import Css exposing (..)
import Css.Global exposing (descendants)
import DateTimePicker.DateTime as DateTime exposing (toMilitary)
import DateTimePicker.DateUtils
import DateTimePicker.Events exposing (onMouseDownPreventDefault, onTouchStartPreventDefault)
import DateTimePicker.Formatter as Formatter
import DateTimePicker.Parser as Parser
import DateTimePicker.Styles as Styles
import DateTimePicker.Svg
import Html.Styled as Html exposing (Html, div, tbody, td, text, tfoot, thead, tr)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.TextInput.V7 as TextInput
import Parser exposing ((|.), (|=), Parser)
import String
import Time


{-| The state of the time picker.
-}
type Model
    = InternalState StateValue


{-| -}
init : Model
init =
    InternalState initialStateValue


type alias StateValue =
    { popupOpen : Bool
    , selectedTime : Maybe Time
    , hourPickerStart : Int
    , minutePickerStart : Int
    , textInputValue : String
    , errorMessage : Maybe String
    }


initialStateValue : StateValue
initialStateValue =
    { popupOpen = False
    , selectedTime = Nothing
    , hourPickerStart = 1
    , minutePickerStart = 0
    , textInputValue = ""
    , errorMessage = Nothing
    }


{-| -}
type alias Time =
    { hour : Int
    , minute : Int
    }


type alias TimeSelection =
    { hour : Maybe Int
    , minute : Maybe Int
    , amPm : Maybe String
    }


{-| -}
view :
    { label : String
    , onChange : Model -> Maybe Time -> msg
    , inputAttributes : List (TextInput.Attribute String msg)
    , state : Model
    , value : Maybe Time
    }
    -> Html msg
view config =
    let
        stateValue =
            case config.state of
                InternalState s ->
                    s

        onChange ( a, b ) =
            config.onChange a b
    in
    Html.node "time-picker"
        [ css [ position relative ] ]
        [ TextInput.view config.label
            ([ TextInput.onFocus (timePickerFocused config stateValue config.value)
             , TextInput.onBlur
                (confirmInputValue config.state config.value
                    |> Tuple.mapFirst closePopup
                    |> onChange
                )
             , TextInput.onEnter (onChange (confirmInputValue config.state config.value))
             , TextInput.text (\newValue -> onChange ( setTextInput newValue stateValue, config.value ))
             , TextInput.value stateValue.textInputValue
             , TextInput.errorMessage stateValue.errorMessage
             ]
                ++ config.inputAttributes
            )
        , if stateValue.popupOpen then
            Html.node "time-picker-dialog"
                [ onMouseDownPreventDefault (config.onChange config.state config.value)
                , css [ display block, Styles.dialog ]
                ]
                [ timePickerDialog config config.state config.value ]

          else
            Html.text ""
        ]


timePickerDialog : { config | onChange : Model -> Maybe Time -> msg } -> Model -> Maybe Time -> Html msg
timePickerDialog config ((InternalState stateValue) as state) currentTime =
    let
        time =
            timeFromTextInputString stateValue.textInputValue

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
                        [ ClickableSvg.onClick (hourUpHandler config stateValue currentTime)
                        , ClickableSvg.exactHeight 24
                        ]
                    ]
                , upArrowTd
                    [ ClickableSvg.button "Earlier minutes"
                        DateTimePicker.Svg.upArrow
                        [ ClickableSvg.onClick (minuteUpHandler config stateValue currentTime)
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
                        [ ClickableSvg.onClick (hourDownHandler config stateValue currentTime)
                        , ClickableSvg.exactHeight 24
                        ]
                    ]
                , downArrowTd
                    [ ClickableSvg.button "Later minutes"
                        DateTimePicker.Svg.downArrow
                        [ ClickableSvg.onClick (minuteDownHandler config stateValue currentTime)
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
            [ Maybe.map toString currentTime
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
    { config | onChange : Model -> Maybe Time -> msg }
    -> StateValue
    -> Maybe Time
    -> msg
hourUpHandler config stateValue currentTime =
    let
        updatedState =
            if stateValue.hourPickerStart - 6 >= 1 then
                { stateValue | hourPickerStart = stateValue.hourPickerStart - 6 }

            else
                stateValue
    in
    config.onChange (updateTextInputFromDate updatedState) currentTime


hourDownHandler :
    { config | onChange : Model -> Maybe Time -> msg }
    -> StateValue
    -> Maybe Time
    -> msg
hourDownHandler config stateValue currentTime =
    let
        updatedState =
            if stateValue.hourPickerStart + 6 <= 12 then
                { stateValue | hourPickerStart = stateValue.hourPickerStart + 6 }

            else
                stateValue
    in
    config.onChange (updateTextInputFromDate updatedState) currentTime


minuteUpHandler :
    { config | onChange : Model -> Maybe Time -> msg }
    -> StateValue
    -> Maybe Time
    -> msg
minuteUpHandler config stateValue currentTime =
    let
        updatedState =
            if stateValue.minutePickerStart - 6 >= 0 then
                { stateValue | minutePickerStart = stateValue.minutePickerStart - 6 }

            else
                stateValue
    in
    config.onChange (updateTextInputFromDate updatedState) currentTime


minuteDownHandler :
    { config | onChange : Model -> Maybe Time -> msg }
    -> StateValue
    -> Maybe Time
    -> msg
minuteDownHandler config stateValue currentTime =
    let
        updatedState =
            if stateValue.minutePickerStart + 6 <= 59 then
                { stateValue | minutePickerStart = stateValue.minutePickerStart + 6 }

            else
                stateValue
    in
    config.onChange (updateTextInputFromDate updatedState) currentTime



-- Cell click handling


cellClickHandler :
    { config | onChange : Model -> Maybe Time -> msg }
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
            stateValue.selectedTime
                |> Maybe.withDefault { hour = 0, minute = 0 }
                |> setHour
                |> setMinute
    in
    config.onChange
        (updateTextInputFromDate
            { stateValue | selectedTime = Just adjustedSelectedDate }
        )
        (Just adjustedSelectedDate)



-- Misc


closePopup : Model -> Model
closePopup (InternalState stateValue) =
    InternalState
        { stateValue
            | hourPickerStart = initialStateValue.hourPickerStart
            , minutePickerStart = initialStateValue.minutePickerStart
            , popupOpen = False
        }


confirmInputValue : Model -> Maybe Time -> ( Model, Maybe Time )
confirmInputValue (InternalState stateValue) currentTime =
    case fromString stateValue.textInputValue of
        Ok selectedTime ->
            ( -- Format the input value the standard way
              updateTextInputFromDate { stateValue | selectedTime = Just selectedTime }
            , Just selectedTime
            )

        Err message ->
            ( InternalState { stateValue | errorMessage = Just message }
            , Nothing
            )


timePickerFocused :
    { config | onChange : Model -> Maybe Time -> msg }
    -> StateValue
    -> Maybe Time
    -> msg
timePickerFocused config stateValue currentTime =
    config.onChange
        (updateTextInputFromDate
            { stateValue
                | popupOpen = True
                , selectedTime = currentTime
            }
        )
        currentTime


setTextInput : String -> StateValue -> Model
setTextInput value state =
    InternalState
        { state
            | textInputValue = value
            , errorMessage = Nothing
        }


updateTextInputFromDate : StateValue -> Model
updateTextInputFromDate state =
    InternalState
        { state
            | textInputValue =
                Maybe.map toString state.selectedTime
                    |> Maybe.withDefault state.textInputValue
        }



-- PARSER


{-| TODO: is the TimeSelection structure actually useful here?
-}
timeFromTextInputString : String -> TimeSelection
timeFromTextInputString textInputValue =
    case fromString textInputValue of
        Ok time ->
            { hour = time.hour |> DateTimePicker.DateUtils.fromMilitaryHour |> Just
            , minute = Just time.minute
            , amPm = time.hour |> DateTimePicker.DateUtils.fromMilitaryAmPm |> Just
            }

        Err _ ->
            { hour = Nothing
            , minute = Nothing
            , amPm = Nothing
            }


fromString : String -> Result String Time
fromString input =
    let
        validater time =
            if time.hour > 23 || time.hour < 0 then
                Parser.problem "Invalid hour"

            else if time.minute > 59 || time.minute < 0 then
                Parser.problem "Invalid minute"

            else
                Parser.succeed time
    in
    Parser.runWithSurroundingSpaceAndValidation timeParser validater input
        |> Result.mapError (toUserFacingErrorMessage input)


toUserFacingErrorMessage : String -> List Parser.DeadEnd -> String
toUserFacingErrorMessage input _ =
    "Failed to parse " ++ input ++ " as a time. Expecting a time in the format hh:mm a.m."


{-| Parse the format "%I:%M %p" and "%I:%M%p"
-}
timeParser : Parser Time
timeParser =
    let
        time_ hour_ minute_ amPm_ =
            { hour = toMilitary hour_ amPm_
            , minute = minute_
            }
    in
    Parser.succeed time_
        |= Parser.clamped 1 12 Parser.looseInt
        |. Parser.skipOptionalSpaces
        |. Parser.symbol ":"
        |. Parser.skipOptionalSpaces
        |= Parser.clamped 0 59 Parser.looseInt
        |. Parser.skipOptionalSpaces
        |= amPm


amPm : Parser String
amPm =
    Parser.oneOf
        [ Parser.map (\_ -> "AM") <|
            Parser.oneOf
                [ Parser.symbol "AM"
                , Parser.symbol "am"
                , Parser.symbol "aM"
                , Parser.symbol "Am"
                , Parser.symbol "a.m."
                , Parser.symbol "A.M."
                ]
        , Parser.map (\_ -> "PM") <|
            Parser.oneOf
                [ Parser.symbol "PM"
                , Parser.symbol "pm"
                , Parser.symbol "pM"
                , Parser.symbol "Pm"
                , Parser.symbol "p.m."
                , Parser.symbol "P.M."
                ]
        ]



-- FORMATTER


{-| -}
toString : Time -> String
toString time =
    let
        ( hourString, amPmString ) =
            if time.hour == 12 then
                ( "12", "p.m." )

            else if time.hour == 0 then
                ( "12", "a.m." )

            else if time.hour > 12 then
                ( Formatter.padWithZero (modBy 12 time.hour), "p.m." )

            else
                ( Formatter.padWithZero time.hour, "a.m." )
    in
    hourString ++ ":" ++ Formatter.padWithZero time.minute ++ " " ++ amPmString
