module DateTimePicker.Formatter
    exposing
        ( accessibilityDateFormatter
        , accessibilityDatePattern
        , dateFormatter
        , datePattern
        , dateTimeFormatter
        , dateTimePattern
        , footerFormatter
        , footerPattern
        , timeFormatter
        , timePattern
        , titleFormatter
        , titlePattern
        )

import Date
import DateTimePicker.DateTime as DateTime


titleFormatter : DateTime.DateTime -> String
titleFormatter dateTime =
    fullMonth dateTime.month
        ++ " "
        ++ toString dateTime.year


titlePattern : String
titlePattern =
    "%B %Y"


accessibilityDateFormatter : DateTime.DateTime -> String
accessibilityDateFormatter dateTime =
    toString dateTime.day
        ++ ", "
        ++ fullDayOfWeek dateTime
        ++ " "
        ++ fullMonth dateTime.month
        ++ " "
        ++ toString dateTime.year


accessibilityDatePattern : String
accessibilityDatePattern =
    "%e, %A %B %Y"


dateFormatter : DateTime.DateTime -> String
dateFormatter dateTime =
    toString dateTime.month ++ "/" ++ toString dateTime.day ++ "/" ++ toString dateTime.year


datePattern : String
datePattern =
    "%m/%d/%Y"


footerFormatter : DateTime.DateTime -> String
footerFormatter dateTime =
    fullDayOfWeek dateTime
        ++ ", "
        ++ fullMonth dateTime.month
        ++ toString dateTime.day
        ++ ", "
        ++ toString dateTime.year


footerPattern : String
footerPattern =
    "%A, %B %d, %Y"


dateTimeFormatter : DateTime.DateTime -> String
dateTimeFormatter dateTime =
    dateFormatter dateTime ++ " " ++ timeFormatter dateTime


dateTimePattern : String
dateTimePattern =
    "%m/%d/%Y %I:%M %p"


timeFormatter : DateTime.DateTime -> String
timeFormatter dateTime =
    let
        ( hourString, amPm ) =
            if dateTime.hour == 12 then
                ( "12", "pm" )
            else if dateTime.hour == 0 then
                ( "12", "am" )
            else if dateTime.hour > 12 then
                ( toString (dateTime.hour % 12), "pm" )
            else
                ( toString dateTime.hour, "am" )
    in
    hourString ++ ":" ++ toString dateTime.minute ++ " " ++ amPm


timePattern : String
timePattern =
    "%I:%M %p"


fullDayOfWeek : DateTime.DateTime -> String
fullDayOfWeek dateTime =
    case DateTime.dayOfWeek dateTime of
        Date.Sun ->
            "Sunday"

        Date.Mon ->
            "Monday"

        Date.Tue ->
            "Tuesday"

        Date.Wed ->
            "Wednewday"

        Date.Thu ->
            "Thursday"

        Date.Fri ->
            "Friday"

        Date.Sat ->
            "Saturday"


fullMonth : Date.Month -> String
fullMonth month =
    case month of
        Date.Jan ->
            "January"

        Date.Feb ->
            "February"

        Date.Mar ->
            "March"

        Date.Apr ->
            "April"

        Date.May ->
            "May"

        Date.Jun ->
            "June"

        Date.Jul ->
            "July"

        Date.Aug ->
            "August"

        Date.Sep ->
            "September"

        Date.Oct ->
            "October"

        Date.Nov ->
            "November"

        Date.Dec ->
            "December"
