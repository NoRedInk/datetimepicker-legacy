module DateUtilsTests exposing (..)

import Date
import Date.Extra.Create
import DateTimePicker.DateUtils as DateUtils
import Expect
import Test exposing (..)


-- TEST SUITE


all : Test
all =
    describe "DateUtils Test Suite"
        [ dayToIntTest
        , generateCalendarTest
        , paddingTest
        , fromMillitaryHourTest
        , fromMillitaryAmPmTest
        , toMillitaryTest
        ]


dayToIntTest : Test
dayToIntTest =
    describe "DateUtils.dayToInt"
        [ test "dayToInt for Sunday when start of week is Sunday should return 0" <|
            \() ->
                DateUtils.dayToInt Date.Sun Date.Sun
                    |> Expect.equal 0
        , test "dayToInt for Friday when start of week is Sunday should return 0" <|
            \() ->
                DateUtils.dayToInt Date.Sun Date.Fri
                    |> Expect.equal 5
        , test "dayToInt for Sunday when start of week is Monday should return 0" <|
            \() ->
                DateUtils.dayToInt Date.Mon Date.Sun
                    |> Expect.equal 6
        , test "dayToInt for Sunday when start of week is Saturday should return 0" <|
            \() ->
                DateUtils.dayToInt Date.Sat Date.Sun
                    |> Expect.equal 1
        ]


generateCalendarTest : Test
generateCalendarTest =
    let
        current =
            DateUtils.Day DateUtils.Current

        previous =
            DateUtils.Day DateUtils.Previous

        next =
            DateUtils.Day DateUtils.Next
    in
    describe "DateUtil.generateCalendar"
        [ test "generateCalendar for February 2016 (leap) should return a list of date" <|
            \() ->
                DateUtils.generateCalendar Date.Sun Date.Feb 2016
                    |> expectDaysAndShape ([ previous 31 ] ++ (List.range 1 29 |> List.map current) ++ (List.range 1 12 |> List.map next))
        , test "generateCalendar for February 2015 should return a list of date" <|
            \() ->
                DateUtils.generateCalendar Date.Sun Date.Feb 2015
                    |> expectDaysAndShape ((List.range 25 31 |> List.map previous) ++ (List.range 1 28 |> List.map current) ++ (List.range 1 7 |> List.map next))
        , test "generateCalendar for January 2099 should return a list of date" <|
            \() ->
                DateUtils.generateCalendar Date.Sun Date.Jan 2099
                    |> expectDaysAndShape ((List.range 28 31 |> List.map previous) ++ (List.range 1 31 |> List.map current) ++ (List.range 1 7 |> List.map next))
        ]


expectDaysAndShape : List DateUtils.Day -> List (List DateUtils.Day) -> Expect.Expectation
expectDaysAndShape days =
    Expect.all
        [ List.concat >> Expect.equal days
        , List.map List.length >> Expect.equal [ 7, 7, 7, 7, 7, 7 ]
        ]


paddingTest : Test
paddingTest =
    describe "DateUtils.padding"
        [ test "padding 1 will return 01" <|
            \() ->
                DateUtils.padding "1"
                    |> Expect.equal "01"
        , test "padding empty string will return 00" <|
            \() ->
                DateUtils.padding ""
                    |> Expect.equal "00"
        , test "padding 12 will return 12" <|
            \() ->
                DateUtils.padding "12"
                    |> Expect.equal "12"
        ]


fromMillitaryHourTest : Test
fromMillitaryHourTest =
    describe "DateUtils.fromMillitaryHour"
        [ test "fromMillitaryHour 12 will return 12" <|
            \() ->
                DateUtils.fromMillitaryHour 12
                    |> Expect.equal 12
        , test "fromMillitaryHour 13 will return 1" <|
            \() ->
                DateUtils.fromMillitaryHour 13
                    |> Expect.equal 1
        , test "fromMillitaryHour 24 will return 12" <|
            \() ->
                DateUtils.fromMillitaryHour 0
                    |> Expect.equal 12
        , test "fromMillitaryHour 23 will return 11" <|
            \() ->
                DateUtils.fromMillitaryHour 23
                    |> Expect.equal 11
        ]


fromMillitaryAmPmTest : Test
fromMillitaryAmPmTest =
    describe "DateUtils.fromMillitaryAmPm"
        [ test "fromMillitaryAmPm 12 will return PM" <|
            \() ->
                DateUtils.fromMillitaryAmPm 12
                    |> Expect.equal "PM"
        , test "fromMillitaryAmPm 0 will return AM" <|
            \() ->
                DateUtils.fromMillitaryAmPm 0
                    |> Expect.equal "AM"
        , test "fromMillitaryAmPm 13 will return PM" <|
            \() ->
                DateUtils.fromMillitaryAmPm 13
                    |> Expect.equal "PM"
        , test "fromMillitaryAmPm 1 will return AM" <|
            \() ->
                DateUtils.fromMillitaryAmPm 1
                    |> Expect.equal "AM"
        , test "fromMillitaryAmPm 23 will return PM" <|
            \() ->
                DateUtils.fromMillitaryAmPm 23
                    |> Expect.equal "PM"
        ]


toMillitaryTest : Test
toMillitaryTest =
    describe "DateUtils.toMillitary"
        [ test "toMillitary 12 AM will return 0" <|
            \() ->
                DateUtils.toMillitary 12 "AM"
                    |> Expect.equal 0
        , test "toMillitary 12 PM will return 12" <|
            \() ->
                DateUtils.toMillitary 12 "PM"
                    |> Expect.equal 12
        , test "toMillitary 9 AM will return 9" <|
            \() ->
                DateUtils.toMillitary 9 "AM"
                    |> Expect.equal 9
        , test "toMillitary 2 PM will return 14" <|
            \() ->
                DateUtils.toMillitary 2 "PM"
                    |> Expect.equal 14
        ]
