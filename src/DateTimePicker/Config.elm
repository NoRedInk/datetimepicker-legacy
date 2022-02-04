module DateTimePicker.Config exposing
    ( NameOfDays
    , defaultDateToInput
    , defaultDateFromInput, defaultNameOfDays
    )

{-|


# THIS IS A HEAVILY MODIFIED FORK OF <https://github.com/abadi199/datetimepicker>


# Configuration

@docs TimePickerConfig, NameOfDays


# Default Configuration

@docs defaultTimePickerConfig, defaultDateTimeToInput, defaultDateToInput

-}

import DateTimePicker.DateTime as DateTime
import DateTimePicker.Formatter as Formatter
import DateTimePicker.Parser as Parser


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
