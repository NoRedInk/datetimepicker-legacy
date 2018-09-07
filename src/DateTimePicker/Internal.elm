module DateTimePicker.Internal
    exposing
        ( InternalState(..)
        , StateValue
        , TimeIndicator(..)
        , TimeSelection
        , getStateValue
        , initialStateValue
        , initialStateValueWithToday
        )

import DateTimePicker.DateTime as DateTime
import DateTimePicker.Geometry exposing (Point)


type InternalState
    = InternalState StateValue


type alias StateValue =
    { inputFocused : Bool
    , forceClose : Bool
    , event : String
    , today : Maybe DateTime.DateTime
    , titleDate : Maybe DateTime.DateTime
    , date : Maybe DateTime.DateTime
    , time : TimeSelection
    , hourPickerStart : Int
    , minutePickerStart : Int
    , currentAngle : Maybe Float
    , activeTimeIndicator : Maybe TimeIndicator
    }


type TimeIndicator
    = HourIndicator
    | MinuteIndicator
    | AMPMIndicator


type alias TimeSelection =
    { hour : Maybe Int, minute : Maybe Int, amPm : Maybe String }


initialStateValue : StateValue
initialStateValue =
    { inputFocused = False
    , forceClose = False
    , event = ""
    , today = Nothing
    , titleDate = Nothing
    , date = Nothing
    , time = TimeSelection Nothing Nothing Nothing
    , hourPickerStart = 1
    , minutePickerStart = 0
    , currentAngle = Nothing
    , activeTimeIndicator = Just HourIndicator
    }


initialStateValueWithToday : DateTime.DateTime -> StateValue
initialStateValueWithToday today =
    { inputFocused = False
    , forceClose = False
    , event = ""
    , today = Just today
    , titleDate = Just <| DateTime.toFirstOfMonth today
    , date = Nothing
    , time = TimeSelection Nothing Nothing Nothing
    , hourPickerStart = 1
    , minutePickerStart = 0
    , currentAngle = Nothing
    , activeTimeIndicator = Just HourIndicator
    }


{-| Get the internal state values
-}
getStateValue : InternalState -> StateValue
getStateValue state =
    case state of
        InternalState stateValue ->
            stateValue
