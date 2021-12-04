module DateTimePicker.DateTime exposing
    ( DateTime
    , addDays
    , addMonths
    , compareDays
    , dayOfWeek
    , daysInMonth
    , fromDate
    , fromParts
    , fromTime
    , intToMonth
    , monthToInt
    , setHour
    , setMinute
    , toFirstOfMonth
    , validate
    )

{-|


# THIS IS A HEAVILY MODIFIED FORK OF <https://github.com/abadi199/datetimepicker>

-}

import Time


type alias DateTime =
    { year : Int, month : Time.Month, day : Int, hour : Int, minute : Int }


compareDays : DateTime -> DateTime -> Order
compareDays left right =
    case Basics.compare left.year right.year of
        EQ ->
            case Basics.compare (monthToInt left.month) (monthToInt right.month) of
                EQ ->
                    Basics.compare left.day right.day

                monthOrder ->
                    monthOrder

        yearOrder ->
            yearOrder


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


intToMonth : Int -> Time.Month
intToMonth month =
    case month of
        1 ->
            Time.Jan

        2 ->
            Time.Feb

        3 ->
            Time.Mar

        4 ->
            Time.Apr

        5 ->
            Time.May

        6 ->
            Time.Jun

        7 ->
            Time.Jul

        8 ->
            Time.Aug

        9 ->
            Time.Sep

        10 ->
            Time.Oct

        11 ->
            Time.Nov

        _ ->
            Time.Dec


dayOfWeek : DateTime -> Time.Weekday
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
                modBy 100 year - 1

            else
                modBy 100 year

        w =
            modBy 7 (k + floor (2.6 * toFloat m - 0.2) - 2 * c + y + floor (toFloat y / 4) + floor (toFloat c / 4))
    in
    case w of
        0 ->
            Time.Sun

        1 ->
            Time.Mon

        2 ->
            Time.Tue

        3 ->
            Time.Wed

        4 ->
            Time.Thu

        5 ->
            Time.Fri

        _ ->
            Time.Sat


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
            intToMonth (modBy 12 wholeMonths + 1)
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


rataDie : Int -> Time.Month -> Int -> Int
rataDie year month day =
    daysBeforeYear year + daysBeforeMonth year month + day


daysBeforeMonth : Int -> Time.Month -> Int
daysBeforeMonth year month =
    let
        leapDays =
            if isLeapYear year then
                1

            else
                0
    in
    case month of
        Time.Jan ->
            0

        Time.Feb ->
            31

        Time.Mar ->
            59 + leapDays

        Time.Apr ->
            90 + leapDays

        Time.May ->
            120 + leapDays

        Time.Jun ->
            151 + leapDays

        Time.Jul ->
            181 + leapDays

        Time.Aug ->
            212 + leapDays

        Time.Sep ->
            243 + leapDays

        Time.Oct ->
            273 + leapDays

        Time.Nov ->
            304 + leapDays

        Time.Dec ->
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
    modBy 4 y == 0 && modBy 100 y /= 0 || modBy 400 y == 0


daysInMonth : Int -> Time.Month -> Int
daysInMonth year month =
    case month of
        Time.Feb ->
            if isLeapYear year then
                29

            else
                28

        Time.Sep ->
            30

        Time.Apr ->
            30

        Time.Jun ->
            30

        Time.Nov ->
            30

        _ ->
            31


validate : DateTime -> Maybe DateTime
validate datetime =
    if datetime.hour > 23 || datetime.hour < 0 then
        Nothing

    else if datetime.minute > 59 || datetime.minute < 0 then
        Nothing

    else if datetime.year < 0 then
        Nothing

    else if datetime.day < 1 then
        Nothing

    else if datetime.day > daysInMonth datetime.year datetime.month then
        Nothing

    else
        Just datetime


fromParts : Int -> Time.Month -> Int -> Int -> Int -> DateTime
fromParts year month day hour minute =
    { year = year
    , month = month
    , day = day
    , hour = hour
    , minute = minute
    }


fromTime : Int -> Int -> String -> DateTime
fromTime hour minute amPm =
    fromParts 0 Time.Jan 1 (toMilitary hour amPm) minute


fromDate : Int -> Time.Month -> Int -> DateTime
fromDate year month day =
    fromParts year month day 0 0


toFirstOfMonth : DateTime -> DateTime
toFirstOfMonth { month, year, hour, minute } =
    { month = month, year = year, day = 1, hour = hour, minute = minute }


setHour : Int -> Maybe String -> DateTime -> DateTime
setHour hour amPm dateTime =
    { dateTime | hour = toMilitary hour (Maybe.withDefault "" amPm) }


setMinute : Int -> DateTime -> DateTime
setMinute minute dateTime =
    { dateTime | minute = minute }


toMilitary : Int -> String -> Int
toMilitary hour amPm =
    case ( hour, amPm ) of
        ( 12, "AM" ) ->
            0

        ( 12, "PM" ) ->
            12

        ( _, "PM" ) ->
            hour + 12

        _ ->
            hour
