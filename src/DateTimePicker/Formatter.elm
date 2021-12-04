module DateTimePicker.Formatter exposing
    ( accessibilityDateFormatter
    , dateFormatter
    , dateTimeFormatter
    , footerFormatter
    , timeFormatter
    , titleFormatter
    )

{-|


# THIS IS A HEAVILY MODIFIED FORK OF <https://github.com/abadi199/datetimepicker>

-}

import DateTimePicker.DateTime as DateTime
import Time


titleFormatter : DateTime.DateTime -> String
titleFormatter dateTime =
    fullMonth dateTime.month
        ++ " "
        ++ String.fromInt dateTime.year


accessibilityDateFormatter : DateTime.DateTime -> String
accessibilityDateFormatter dateTime =
    String.fromInt dateTime.day
        ++ ", "
        ++ fullDayOfWeek dateTime
        ++ " "
        ++ fullMonth dateTime.month
        ++ " "
        ++ String.fromInt dateTime.year


dateFormatter : DateTime.DateTime -> String
dateFormatter dateTime =
    padWithZero (DateTime.monthToInt dateTime.month) ++ "/" ++ padWithZero dateTime.day ++ "/" ++ String.fromInt dateTime.year


footerFormatter : DateTime.DateTime -> String
footerFormatter dateTime =
    fullDayOfWeek dateTime
        ++ ", "
        ++ fullMonth dateTime.month
        ++ " "
        ++ String.fromInt dateTime.day
        ++ ", "
        ++ String.fromInt dateTime.year


dateTimeFormatter : DateTime.DateTime -> String
dateTimeFormatter dateTime =
    dateFormatter dateTime ++ " " ++ timeFormatter dateTime


timeFormatter : DateTime.DateTime -> String
timeFormatter dateTime =
    let
        ( hourString, amPm ) =
            if dateTime.hour == 12 then
                ( "12", "p.m." )

            else if dateTime.hour == 0 then
                ( "12", "a.m." )

            else if dateTime.hour > 12 then
                ( padWithZero (modBy 12 dateTime.hour), "p.m." )

            else
                ( padWithZero dateTime.hour, "a.m." )
    in
    hourString ++ ":" ++ padWithZero dateTime.minute ++ " " ++ amPm


fullDayOfWeek : DateTime.DateTime -> String
fullDayOfWeek dateTime =
    case DateTime.dayOfWeek dateTime of
        Time.Sun ->
            "Sunday"

        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"


fullMonth : Time.Month -> String
fullMonth month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


padWithZero : Int -> String
padWithZero input =
    if input < 10 then
        "0" ++ String.fromInt input

    else
        String.fromInt input
