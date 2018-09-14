module DateTimePicker.Formatter
    exposing
        ( accessibilityDateFormatter
        , dateFormatter
        , dateTimeFormatter
        , footerFormatter
        , timeFormatter
        , titleFormatter
        )

import Date
import DateTimePicker.DateTime as DateTime


titleFormatter : DateTime.DateTime -> String
titleFormatter dateTime =
    fullMonth dateTime.month
        ++ " "
        ++ toString dateTime.year


accessibilityDateFormatter : DateTime.DateTime -> String
accessibilityDateFormatter dateTime =
    toString dateTime.day
        ++ ", "
        ++ fullDayOfWeek dateTime
        ++ " "
        ++ fullMonth dateTime.month
        ++ " "
        ++ toString dateTime.year


dateFormatter : DateTime.DateTime -> String
dateFormatter dateTime =
    toString (DateTime.monthToInt dateTime.month) ++ "/" ++ toString dateTime.day ++ "/" ++ toString dateTime.year


footerFormatter : DateTime.DateTime -> String
footerFormatter dateTime =
    fullDayOfWeek dateTime
        ++ ", "
        ++ fullMonth dateTime.month
        ++ " "
        ++ toString dateTime.day
        ++ ", "
        ++ toString dateTime.year


dateTimeFormatter : DateTime.DateTime -> String
dateTimeFormatter dateTime =
    dateFormatter dateTime ++ " " ++ timeFormatter dateTime


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
