module DateUtilsTests exposing (all, dayToIntTest, fromMilitaryAmPmTest, fromMilitaryHourTest, generateCalendarTest, paddingTest, toMilitaryTest)

import DateTimePicker.DateUtils as DateUtils
import Expect
import Test exposing (..)
import Time



-- TEST SUITE


all : Test
all =
    describe "DateUtils Test Suite"
        [ dayToIntTest
        , generateCalendarTest
        , paddingTest
        , fromMilitaryHourTest
        , fromMilitaryAmPmTest
        , toMilitaryTest
        ]


dayToIntTest : Test
dayToIntTest =
    describe "DateUtils.dayToInt"
        [ test "dayToInt for Sunday when start of week is Sunday should return 0" <|
            \() ->
                DateUtils.dayToInt Time.Sun Time.Sun
                    |> Expect.equal 0
        , test "dayToInt for Friday when start of week is Sunday should return 0" <|
            \() ->
                DateUtils.dayToInt Time.Sun Time.Fri
                    |> Expect.equal 5
        , test "dayToInt for Sunday when start of week is Monday should return 0" <|
            \() ->
                DateUtils.dayToInt Time.Mon Time.Sun
                    |> Expect.equal 6
        , test "dayToInt for Sunday when start of week is Saturday should return 0" <|
            \() ->
                DateUtils.dayToInt Time.Sat Time.Sun
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
                DateUtils.generateCalendar Time.Sun Time.Feb 2016
                    |> expectDaysAndShape ([ previous 31 ] ++ (List.range 1 29 |> List.map current) ++ (List.range 1 12 |> List.map next))
        , test "generateCalendar for February 2015 should return a list of date" <|
            \() ->
                DateUtils.generateCalendar Time.Sun Time.Feb 2015
                    |> expectDaysAndShape ((List.range 25 31 |> List.map previous) ++ (List.range 1 28 |> List.map current) ++ (List.range 1 7 |> List.map next))
        , test "generateCalendar for January 2099 should return a list of date" <|
            \() ->
                DateUtils.generateCalendar Time.Sun Time.Jan 2099
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


fromMilitaryHourTest : Test
fromMilitaryHourTest =
    describe "DateUtils.fromMilitaryHour"
        [ test "fromMilitaryHour 12 will return 12" <|
            \() ->
                DateUtils.fromMilitaryHour 12
                    |> Expect.equal 12
        , test "fromMilitaryHour 13 will return 1" <|
            \() ->
                DateUtils.fromMilitaryHour 13
                    |> Expect.equal 1
        , test "fromMilitaryHour 24 will return 12" <|
            \() ->
                DateUtils.fromMilitaryHour 0
                    |> Expect.equal 12
        , test "fromMilitaryHour 23 will return 11" <|
            \() ->
                DateUtils.fromMilitaryHour 23
                    |> Expect.equal 11
        ]


fromMilitaryAmPmTest : Test
fromMilitaryAmPmTest =
    describe "DateUtils.fromMilitaryAmPm"
        [ test "fromMilitaryAmPm 12 will return PM" <|
            \() ->
                DateUtils.fromMilitaryAmPm 12
                    |> Expect.equal "PM"
        , test "fromMilitaryAmPm 0 will return AM" <|
            \() ->
                DateUtils.fromMilitaryAmPm 0
                    |> Expect.equal "AM"
        , test "fromMilitaryAmPm 13 will return PM" <|
            \() ->
                DateUtils.fromMilitaryAmPm 13
                    |> Expect.equal "PM"
        , test "fromMilitaryAmPm 1 will return AM" <|
            \() ->
                DateUtils.fromMilitaryAmPm 1
                    |> Expect.equal "AM"
        , test "fromMilitaryAmPm 23 will return PM" <|
            \() ->
                DateUtils.fromMilitaryAmPm 23
                    |> Expect.equal "PM"
        ]


toMilitaryTest : Test
toMilitaryTest =
    describe "DateUtils.toMilitary"
        [ test "toMilitary 12 AM will return 0" <|
            \() ->
                DateUtils.toMilitary 12 "AM"
                    |> Expect.equal 0
        , test "toMilitary 12 PM will return 12" <|
            \() ->
                DateUtils.toMilitary 12 "PM"
                    |> Expect.equal 12
        , test "toMilitary 9 AM will return 9" <|
            \() ->
                DateUtils.toMilitary 9 "AM"
                    |> Expect.equal 9
        , test "toMilitary 2 PM will return 14" <|
            \() ->
                DateUtils.toMilitary 2 "PM"
                    |> Expect.equal 14
        ]
