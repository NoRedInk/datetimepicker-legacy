module DateTimePicker.Internal exposing
    ( DateSelection
    , InternalState(..)
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
import DateTimePicker.DateUtils
import Time


type InternalState
    = InternalState StateValue


type alias StateValue =
    { inputFocused : Bool
    , today : Maybe DateTime.DateTime
    , titleDate : Maybe DateTime.DateTime
    , date : Maybe DateTime.DateTime
    , hourPickerStart : Int
    , minutePickerStart : Int
    , textInputValue : String
    }


type TimeIndicator
    = HourIndicator
    | MinuteIndicator
    | AMPMIndicator


type alias DateSelection =
    { year : Int
    , month : Time.Month
    , day : DateTimePicker.DateUtils.Day
    }


type alias TimeSelection =
    { hour : Maybe Int
    , minute : Maybe Int
    , amPm : Maybe String
    }


initialStateValue : StateValue
initialStateValue =
    { inputFocused = False
    , today = Nothing
    , titleDate = Nothing
    , date = Nothing
    , hourPickerStart = 1
    , minutePickerStart = 0
    , textInputValue = ""
    }


initialStateValueWithToday : DateTime.DateTime -> StateValue
initialStateValueWithToday today =
    { inputFocused = False
    , today = Just today
    , titleDate = Just <| DateTime.toFirstOfMonth today
    , date = Nothing
    , hourPickerStart = 1
    , minutePickerStart = 0
    , textInputValue = ""
    }


{-| Get the internal state values
-}
getStateValue : InternalState -> StateValue
getStateValue state =
    case state of
        InternalState stateValue ->
            stateValue
