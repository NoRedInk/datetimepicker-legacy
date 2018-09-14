module DateTimePicker.Config
    exposing
        ( Config
        , DatePickerConfig
        , NameOfDays
        , TimePickerConfig
        , TimePickerType(..)
        , Type(..)
        , defaultDatePickerConfig
        , defaultDateTimePickerConfig
        , defaultParseDate
        , defaultParseDateTime
        , defaultParseTime
        , defaultTimePickerConfig
        )

{-| DateTimePicker configuration


# Configuration

@docs Config, DatePickerConfig, TimePickerConfig, NameOfDays, TimePickerType, Type


# Default Configuration

@docs defaultDatePickerConfig, defaultTimePickerConfig, defaultDateTimePickerConfig, defaultParseDate, defaultParseTime, defaultParseDateTime

-}

import Date
import DateTimePicker.DateTime as DateTime
import DateTimePicker.Internal exposing (InternalState)
import DateTimePicker.Parser as Parser
import Html.Styled as Html


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
        , parseInput : String -> Maybe DateTime.DateTime
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
        , firstDayOfWeek : Date.Day
        , allowYearNavigation : Bool
        , earliestDate : Maybe DateTime.DateTime
    }


{-| Configuration for TimePicker

  - `timePickerType` is the type of the time picker, either Analog or Digital

-}
type alias TimePickerConfig =
    { timePickerType : TimePickerType
    }


{-| The type of time picker, can be either Digital or Analog
-}
type TimePickerType
    = Digital
    | Analog


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
    , firstDayOfWeek = Date.Sun
    , allowYearNavigation = True
    , usePicker = True
    , attributes = []
    , earliestDate = Nothing
    , parseInput = Parser.parseDate
    }


{-| Default configuration for TimePicker

  - `onChange` No Default
  - `dateFormatter` Default: `"%m/%d/%Y"`
  - `dateTimeFormatter` Default: `"%m/%d/%Y %I:%M %p"`
  - `autoClose` Default: False
  - `timeFormatter` Default: `"%I:%M %p"`
  - `timePickerType` Default: Analog

-}
defaultTimePickerConfig : (State -> Maybe DateTime.DateTime -> msg) -> Config TimePickerConfig msg
defaultTimePickerConfig onChange =
    { onChange = onChange
    , autoClose = False
    , timePickerType = Analog
    , usePicker = True
    , attributes = []
    , parseInput = Parser.parseTime
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
  - `timePickerType` Default: Analog
  - `allowYearNavigation` Default : True
  - `earliestDate` Default : Nothing

-}
defaultDateTimePickerConfig : (State -> Maybe DateTime.DateTime -> msg) -> Config (DatePickerConfig TimePickerConfig) msg
defaultDateTimePickerConfig onChange =
    { onChange = onChange
    , autoClose = False
    , nameOfDays = defaultNameOfDays
    , firstDayOfWeek = Date.Sun
    , timePickerType = Analog
    , allowYearNavigation = True
    , usePicker = True
    , attributes = []
    , earliestDate = Nothing
    , parseInput = Parser.parseDateTime
    }


{-| Default date parser
-}
defaultParseDate : String -> Maybe DateTime.DateTime
defaultParseDate =
    Parser.parseDate


{-| Default time parser
-}
defaultParseTime : String -> Maybe DateTime.DateTime
defaultParseTime =
    Parser.parseTime


{-| Default date and time parser
-}
defaultParseDateTime : String -> Maybe DateTime.DateTime
defaultParseDateTime =
    Parser.parseDateTime


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
