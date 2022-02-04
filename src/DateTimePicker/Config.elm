module DateTimePicker.Config exposing
    ( TimePickerConfig, DatePickerConfig, NameOfDays, Type(..)
    , defaultDatePickerConfig, defaultTimePickerConfig, defaultDateFromInput, defaultTimeFromInput, defaultDateTimeFromInput, defaultDateTimeToInput, defaultDateToInput, defaultTimeToInput
    )

{-|


# THIS IS A HEAVILY MODIFIED FORK OF <https://github.com/abadi199/datetimepicker>


# Configuration

@docs TimePickerConfig, DatePickerConfig, NameOfDays, Type


# Default Configuration

@docs defaultDatePickerConfig, defaultTimePickerConfig, defaultDateFromInput, defaultTimeFromInput, defaultDateTimeFromInput, defaultDateTimeToInput, defaultDateToInput, defaultTimeToInput

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
    = DateType (DatePickerConfig msg)
    | TimeType (TimePickerConfig msg)


{-| Configuration for the DatePicker

  - `nameOfDays` is the configuration for name of days in a week.
  - `firstDayOfWeek` is the first day of the week.
  - `allowYearNavigation` show/hide year navigation button.
  - `earliestDate` if given, dates before this cannot be selected

-}
type alias DatePickerConfig msg =
    { nameOfDays : NameOfDays
    , firstDayOfWeek : Time.Weekday
    , allowYearNavigation : Bool
    , earliestDate : Maybe DateTime.DateTime
    , onChange : State -> Maybe DateTime.DateTime -> msg
    , usePicker : Bool
    , attributes : List (Html.Attribute msg)
    , fromInput : String -> Maybe DateTime.DateTime
    , toInput : DateTime.DateTime -> String
    }


{-| Default configuration for DatePicker

  - `onChange` No Default
  - `nameOfDays` see `NameOfDays` for the default values.
  - `firstDayOfWeek` Default: Sunday.
  - `allowYearNavigation` Default : True
  - `earliestDate` Default : Nothing

-}
defaultDatePickerConfig : (State -> Maybe DateTime.DateTime -> msg) -> DatePickerConfig msg
defaultDatePickerConfig onChange =
    { onChange = onChange
    , nameOfDays = defaultNameOfDays
    , firstDayOfWeek = Time.Sun
    , allowYearNavigation = True
    , usePicker = True
    , attributes = []
    , earliestDate = Nothing
    , fromInput = Parser.parseDate
    , toInput = Formatter.dateFormatter
    }


type alias TimePickerConfig msg =
    { onChange : State -> Maybe DateTime.DateTime -> msg
    , usePicker : Bool
    , attributes : List (Html.Attribute msg)
    , fromInput : String -> Maybe DateTime.DateTime
    , toInput : DateTime.DateTime -> String
    }


{-| Default configuration for TimePicker

  - `onChange` No Default
  - `dateFormatter` Default: `"%m/%d/%Y"`
  - `dateTimeFormatter` Default: `"%m/%d/%Y %I:%M %p"`
  - `timeFormatter` Default: `"%I:%M %p"`

-}
defaultTimePickerConfig : (State -> Maybe DateTime.DateTime -> msg) -> TimePickerConfig msg
defaultTimePickerConfig onChange =
    { onChange = onChange
    , usePicker = True
    , attributes = []
    , fromInput = Parser.parseTime
    , toInput = Formatter.timeFormatter
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
