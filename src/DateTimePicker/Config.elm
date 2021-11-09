module DateTimePicker.Config exposing
    ( Config, DatePickerConfig, TimePickerConfig, NameOfDays, Type(..)
    , defaultDatePickerConfig, defaultTimePickerConfig, defaultDateTimePickerConfig, defaultDateFromInput, defaultTimeFromInput, defaultDateTimeFromInput, defaultDateTimeToInput, defaultDateToInput, defaultTimeToInput
    )

{-|


# THIS IS A HEAVILY MODIFIED FORK OF <https://github.com/abadi199/datetimepicker>


# Configuration

@docs Config, DatePickerConfig, TimePickerConfig, NameOfDays, Type


# Default Configuration

@docs defaultDatePickerConfig, defaultTimePickerConfig, defaultDateTimePickerConfig, defaultDateFromInput, defaultTimeFromInput, defaultDateTimeFromInput, defaultDateTimeToInput, defaultDateToInput, defaultTimeToInput

-}

import DateTimePicker.DateTime as DateTime
import DateTimePicker.Formatter as Formatter
import DateTimePicker.Internal exposing (InternalState)
import DateTimePicker.Parser as Parser
import Html.Styled as Html
import Time


type alias State =
    InternalState


{-| The type of picker (for Internal Use)
-}
type Type msg
    = DateType (Config (DatePickerConfig {}) msg)
    | DateTimeType (Config (DatePickerConfig TimePickerConfig) msg)
    | TimeType (Config TimePickerConfig msg)


{-| Configuration

  - `onChange` is the message for when the selected value and internal `State` in the date picker has changed.
  - `autoClose` is a flag to indicate whether the dialog should be automatically closed when a date and/or time is selected.
  - `parseInput` accepts a user string from the input element and attempts to convert it to a DateTime

-}
type alias Config otherConfig msg =
    { otherConfig
        | onChange : State -> Maybe DateTime.DateTime -> msg
        , autoClose : Bool
        , usePicker : Bool
        , attributes : List (Html.Attribute msg)
        , fromInput : String -> Maybe DateTime.DateTime
        , toInput : DateTime.DateTime -> String
    }


{-| Configuration for the DatePicker

  - `nameOfDays` is the configuration for name of days in a week.
  - `firstDayOfWeek` is the first day of the week.
  - `allowYearNavigation` show/hide year navigation button.
  - `earliestDate` if given, dates before this cannot be selected

-}
type alias DatePickerConfig otherConfig =
    { otherConfig
        | nameOfDays : NameOfDays
        , firstDayOfWeek : Time.Weekday
        , allowYearNavigation : Bool
        , earliestDate : Maybe DateTime.DateTime
    }


{-| Configuration for TimePicker
-}
type alias TimePickerConfig =
    {}


{-| Default configuration for DatePicker

  - `onChange` No Default
  - `autoClose` Default: True
  - `nameOfDays` see `NameOfDays` for the default values.
  - `firstDayOfWeek` Default: Sunday.
  - `allowYearNavigation` Default : True
  - `earliestDate` Default : Nothing

-}
defaultDatePickerConfig : (State -> Maybe DateTime.DateTime -> msg) -> Config (DatePickerConfig {}) msg
defaultDatePickerConfig onChange =
    { onChange = onChange
    , autoClose = True
    , nameOfDays = defaultNameOfDays
    , firstDayOfWeek = Time.Sun
    , allowYearNavigation = True
    , usePicker = True
    , attributes = []
    , earliestDate = Nothing
    , fromInput = Parser.parseDate
    , toInput = Formatter.dateFormatter
    }


{-| Default configuration for TimePicker

  - `onChange` No Default
  - `dateFormatter` Default: `"%m/%d/%Y"`
  - `dateTimeFormatter` Default: `"%m/%d/%Y %I:%M %p"`
  - `autoClose` Default: False
  - `timeFormatter` Default: `"%I:%M %p"`

-}
defaultTimePickerConfig : (State -> Maybe DateTime.DateTime -> msg) -> Config TimePickerConfig msg
defaultTimePickerConfig onChange =
    { onChange = onChange
    , autoClose = False
    , usePicker = True
    , attributes = []
    , fromInput = Parser.parseTime
    , toInput = Formatter.timeFormatter
    }


{-| Default configuration for DateTimePicker

  - `onChange` No Default
  - `dateFormatter` Default: `"%m/%d/%Y"`
  - `dateTimeFormatter` Default: `"%m/%d/%Y %I:%M %p"`
  - `autoClose` Default: False
  - `nameOfDays` see `NameOfDays` for the default values.
  - `firstDayOfWeek` Default: Sunday.
  - `titleFormatter` Default: `"%B %Y"`
  - `fullDateFormatter` Default: `"%A, %B %d, %Y"`
  - `timeFormatter` Default: `"%I:%M %p"`
  - `allowYearNavigation` Default : True
  - `earliestDate` Default : Nothing

-}
defaultDateTimePickerConfig : (State -> Maybe DateTime.DateTime -> msg) -> Config (DatePickerConfig TimePickerConfig) msg
defaultDateTimePickerConfig onChange =
    { onChange = onChange
    , autoClose = False
    , nameOfDays = defaultNameOfDays
    , firstDayOfWeek = Time.Sun
    , allowYearNavigation = True
    , usePicker = True
    , attributes = []
    , earliestDate = Nothing
    , fromInput = Parser.parseDateTime
    , toInput = Formatter.dateTimeFormatter
    }


{-| Default date parser
-}
defaultDateFromInput : String -> Maybe DateTime.DateTime
defaultDateFromInput =
    Parser.parseDate


{-| Default date printer
-}
defaultDateToInput : DateTime.DateTime -> String
defaultDateToInput =
    Formatter.dateFormatter


{-| Default time parser
-}
defaultTimeFromInput : String -> Maybe DateTime.DateTime
defaultTimeFromInput =
    Parser.parseTime


{-| Default time printer
-}
defaultTimeToInput : DateTime.DateTime -> String
defaultTimeToInput =
    Formatter.timeFormatter


{-| Default date and time parser
-}
defaultDateTimeFromInput : String -> Maybe DateTime.DateTime
defaultDateTimeFromInput =
    Parser.parseDateTime


{-| Default date and time printer
-}
defaultDateTimeToInput : DateTime.DateTime -> String
defaultDateTimeToInput =
    Formatter.dateTimeFormatter


{-| Configuration for name of days in a week.

This will be displayed as the calendar's header.
Default:

  - sunday = "Su"
  - monday = "Mo"
  - tuesday = "Tu"
  - wednesday = "We"
  - thursday = "Th"
  - friday = "Fr"
  - saturday = "Sa"

-}
type alias NameOfDays =
    { sunday : String
    , monday : String
    , tuesday : String
    , wednesday : String
    , thursday : String
    , friday : String
    , saturday : String
    }


defaultNameOfDays : NameOfDays
defaultNameOfDays =
    { sunday = "Su"
    , monday = "Mo"
    , tuesday = "Tu"
    , wednesday = "We"
    , thursday = "Th"
    , friday = "Fr"
    , saturday = "Sa"
    }
