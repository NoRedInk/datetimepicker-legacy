module DateTimePicker.Internal exposing
    ( InternalState(..)
    , StateValue
    , TimeIndicator(..)
    , TimeSelection
    , getStateValue
    , initialStateValue
    , initialStateValueWithToday
    )

{-|


# THIS IS A HEAVILY MODIFIED FORK OF <https://github.com/abadi199/datetimepicker>

-}

import DateTimePicker.DateTime as DateTime


type InternalState
    = InternalState StateValue


type alias StateValue =
    { inputFocused : Bool
    , forceClose : Bool
    , today : Maybe DateTime.DateTime
    , titleDate : Maybe DateTime.DateTime
    , date : Maybe DateTime.DateTime
    , time : TimeSelection
    , hourPickerStart : Int
    , minutePickerStart : Int
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
    , today = Nothing
    , titleDate = Nothing
    , date = Nothing
    , time = TimeSelection Nothing Nothing Nothing
    , hourPickerStart = 1
    , minutePickerStart = 0
    , activeTimeIndicator = Just HourIndicator
    }


initialStateValueWithToday : DateTime.DateTime -> StateValue
initialStateValueWithToday today =
    { inputFocused = False
    , forceClose = False
    , today = Just today
    , titleDate = Just <| DateTime.toFirstOfMonth today
    , date = Nothing
    , time = TimeSelection Nothing Nothing Nothing
    , hourPickerStart = 1
    , minutePickerStart = 0
    , activeTimeIndicator = Just HourIndicator
    }


{-| Get the internal state values
-}
getStateValue : InternalState -> StateValue
getStateValue state =
    case state of
        InternalState stateValue ->
            stateValue
