module DateTimePicker.DateTime
    exposing
        ( DateTime
        , addDays
        , addMonths
        , dayOfWeek
        , daysInMonth
        , fromDate
        , fromParts
        , fromTime
        , intToMonth
        , monthToInt
        , now
        , setTime
        , toFirstOfMonth
        )

import Date
import Task exposing (Task)


type alias DateTime =
    { year : Int, month : Date.Month, day : Int, hour : Int, minute : Int }


now : Task never DateTime
now =
    Task.map
        (\date ->
            { year = Date.year date
            , month = Date.month date
            , day = Date.day date
            , hour = Date.hour date
            , minute = Date.minute date
            }
        )
        Date.now


monthToInt : Date.Month -> Int
monthToInt month =
    case month of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12


intToMonth : Int -> Date.Month
intToMonth month =
    case month of
        1 ->
            Date.Jan

        2 ->
            Date.Feb

        3 ->
            Date.Mar

        4 ->
            Date.Apr

        5 ->
            Date.May

        6 ->
            Date.Jun

        7 ->
            Date.Jul

        8 ->
            Date.Aug

        9 ->
            Date.Sep

        10 ->
            Date.Oct

        11 ->
            Date.Nov

        _ ->
            Date.Dec


dayOfWeek : DateTime -> Date.Day
dayOfWeek { year, month, day } =
    let
        om =
            monthToInt month

        k =
            day

        m =
            if om <= 2 then
                om + 10
            else
                om - 2

        c =
            floor (toFloat year / 100)

        y =
            if om <= 2 then
                year % 100 - 1
            else
                year % 100

        w =
            (k + floor (2.6 * toFloat m - 0.2) - 2 * c + y + floor (toFloat y / 4) + floor (toFloat c / 4)) % 7
    in
    case w of
        0 ->
            Date.Sun

        1 ->
            Date.Mon

        2 ->
            Date.Tue

        3 ->
            Date.Wed

        4 ->
            Date.Thu

        5 ->
            Date.Fri

        _ ->
            Date.Sat


addDays : Int -> DateTime -> DateTime
addDays days date =
    let
        rataDays =
            rataDie date.year date.month (date.day + days)

        posixDays =
            rataDays - 719163

        posixMinutes =
            posixDays * 24 * 60

        { day, month, year } =
            fromPosixMinutes posixMinutes
    in
    { day = day
    , month = intToMonth month
    , year = year
    , hour = date.hour
    , minute = date.minute
    }


addMonths : Int -> DateTime -> DateTime
addMonths n date =
    let
        wholeMonths =
            12 * (date.year - 1) + monthToInt date.month - 1 + n

        y =
            wholeMonths // 12 + 1

        m =
            intToMonth ((wholeMonths % 12) + 1)
    in
    fromParts y m (min date.day (daysInMonth y m)) date.hour date.minute


fromPosixMinutes : Int -> { year : Int, month : Int, day : Int }
fromPosixMinutes minutes =
    let
        rawDay =
            flooredDiv minutes (60 * 24) + 719468

        era =
            (if rawDay >= 0 then
                rawDay
             else
                rawDay - 146096
            )
                // 146097

        dayOfEra =
            rawDay - era * 146097

        -- [0, 146096]
        yearOfEra =
            (dayOfEra - dayOfEra // 1460 + dayOfEra // 36524 - dayOfEra // 146096) // 365

        -- [0, 399]
        year =
            yearOfEra + era * 400

        dayOfYear =
            dayOfEra - (365 * yearOfEra + yearOfEra // 4 - yearOfEra // 100)

        -- [0, 365]
        mp =
            (5 * dayOfYear + 2) // 153

        -- [0, 11]
        month =
            mp
                + (if mp < 10 then
                    3
                   else
                    -9
                  )

        -- [1, 12]
    in
    { year =
        year
            + (if month <= 2 then
                1
               else
                0
              )
    , month = month
    , day = dayOfYear - (153 * mp + 2) // 5 + 1 -- [1, 31]
    }


flooredDiv : Int -> Float -> Int
flooredDiv numerator denominator =
    floor (toFloat numerator / denominator)


rataDie : Int -> Date.Month -> Int -> Int
rataDie year month day =
    daysBeforeYear year + daysBeforeMonth year month + day


daysBeforeMonth : Int -> Date.Month -> Int
daysBeforeMonth year month =
    let
        leapDays =
            if isLeapYear year then
                1
            else
                0
    in
    case month of
        Date.Jan ->
            0

        Date.Feb ->
            31

        Date.Mar ->
            59 + leapDays

        Date.Apr ->
            90 + leapDays

        Date.May ->
            120 + leapDays

        Date.Jun ->
            151 + leapDays

        Date.Jul ->
            181 + leapDays

        Date.Aug ->
            212 + leapDays

        Date.Sep ->
            243 + leapDays

        Date.Oct ->
            273 + leapDays

        Date.Nov ->
            304 + leapDays

        Date.Dec ->
            334 + leapDays


daysBeforeYear : Int -> Int
daysBeforeYear y1 =
    let
        y =
            y1 - 1

        leapYears =
            (y // 4) - (y // 100) + (y // 400)
    in
    365 * y + leapYears


isLeapYear : Int -> Bool
isLeapYear y =
    y % 4 == 0 && y % 100 /= 0 || y % 400 == 0


daysInMonth : Int -> Date.Month -> Int
daysInMonth year month =
    if month == Date.Feb && year % 4 == 0 then
        29
    else if month == Date.Feb && year % 4 /= 0 then
        28
    else if month == Date.Sep || month == Date.Apr || month == Date.Jun || month == Date.Nov then
        30
    else
        31


fromParts : Int -> Date.Month -> Int -> Int -> Int -> DateTime
fromParts year month day hour minute =
    { year = year
    , month = month
    , day = day
    , hour = hour
    , minute = minute
    }


fromTime : Int -> Int -> String -> DateTime
fromTime hour minute amPm =
    fromParts 0 Date.Jan 1 (toMillitary hour amPm) minute


fromDate : Int -> Date.Month -> Int -> DateTime
fromDate year month day =
    fromParts year month day 0 0


toFirstOfMonth : DateTime -> DateTime
toFirstOfMonth { month, year, hour, minute } =
    { month = month, year = year, day = 1, hour = hour, minute = minute }


setTime : Int -> Int -> String -> DateTime -> DateTime
setTime hour minute amPm dateTime =
    { dateTime | hour = toMillitary hour amPm, minute = minute }


toMillitary : Int -> String -> Int
toMillitary hour amPm =
    case ( hour, amPm ) of
        ( 12, "AM" ) ->
            0

        ( 12, "PM" ) ->
            12

        ( _, "PM" ) ->
            hour + 12

        ( _, _ ) ->
            hour
