module DateTimeTests exposing (addDaysTest, addMonthsTest, dayOfWeekTest, firstOfMonthTest, setTimeTest, validateTest)

import DateTimePicker.DateTime as DateTime
import Expect exposing (Expectation)
import Test exposing (..)
import Time


firstOfMonthTest : Test
firstOfMonthTest =
    describe "firstOfMonth"
        [ test "it should always zero out the month" <|
            \() ->
                DateTime.fromParts 2018 Time.Sep 6 10 49
                    |> DateTime.toFirstOfMonth
                    |> Expect.equal (DateTime.fromParts 2018 Time.Sep 1 10 49)
        ]


setTimeTest : Test
setTimeTest =
    let
        date =
            DateTime.fromParts 2018 Time.Sep 6 10 49
    in
    describe "DateTime.setTime"
        [ test "setTime for 12:00 AM should return the right time" <|
            \() ->
                DateTime.setTime 12 0 "AM" date
                    |> Expect.equal (DateTime.fromParts 2018 Time.Sep 6 0 0)
        , test "setTime for 12:00 PM should return the right time" <|
            \() ->
                DateTime.setTime 12 0 "PM" date
                    |> Expect.equal (DateTime.fromParts 2018 Time.Sep 6 12 0)
        , test "setTime for 3:15 PM should return the right time" <|
            \() ->
                DateTime.setTime 3 15 "PM" date
                    |> Expect.equal (DateTime.fromParts 2018 Time.Sep 6 15 15)
        , test "setTime for 3:15 AM should return the right time" <|
            \() ->
                DateTime.setTime 3 15 "AM" date
                    |> Expect.equal (DateTime.fromParts 2018 Time.Sep 6 3 15)
        ]


addDaysTest : Test
addDaysTest =
    describe "DateTime.addDays"
        [ test "forward in the middle of a month" <|
            \() ->
                DateTime.fromParts 2018 Time.Sep 6 10 49
                    |> DateTime.addDays 1
                    |> Expect.equal (DateTime.fromParts 2018 Time.Sep 7 10 49)
        , test "backward in the middle of a month" <|
            \() ->
                DateTime.fromParts 2018 Time.Sep 6 10 49
                    |> DateTime.addDays -1
                    |> Expect.equal (DateTime.fromParts 2018 Time.Sep 5 10 49)
        , test "forward past the end of the month" <|
            \() ->
                DateTime.fromParts 2018 Time.Sep 30 10 49
                    |> DateTime.addDays 1
                    |> Expect.equal (DateTime.fromParts 2018 Time.Oct 1 10 49)
        , test "backward past the beginning of the month" <|
            \() ->
                DateTime.fromParts 2018 Time.Sep 1 10 49
                    |> DateTime.addDays -1
                    |> Expect.equal (DateTime.fromParts 2018 Time.Aug 31 10 49)
        , test "foward accross several months" <|
            \() ->
                DateTime.fromParts 2018 Time.Sep 1 10 49
                    |> DateTime.addDays 75
                    |> Expect.equal (DateTime.fromParts 2018 Time.Nov 15 10 49)
        , test "backward accross several months" <|
            \() ->
                DateTime.fromParts 2018 Time.Sep 1 10 49
                    |> DateTime.addDays -75
                    |> Expect.equal (DateTime.fromParts 2018 Time.Jun 18 10 49)
        ]


addMonthsTest : Test
addMonthsTest =
    test "addMonths" <|
        \() ->
            let
                start =
                    DateTime.fromParts 1999 Time.Dec 31 23 59

                inputsAndExpecteds =
                    [ ( 1, DateTime.fromParts 2000 Time.Jan 31 23 59 )
                    , ( 2, DateTime.fromParts 2000 Time.Feb 29 23 59 )
                    , ( 4, DateTime.fromParts 2000 Time.Apr 30 23 59 )
                    , ( 14, DateTime.fromParts 2001 Time.Feb 28 23 59 )
                    ]
            in
            Expect.all
                (List.map
                    (\( monthsToAdd, expected ) ->
                        \startDate -> Expect.equal expected (DateTime.addMonths monthsToAdd startDate)
                    )
                    inputsAndExpecteds
                )
                start


dayOfWeekTest : Test
dayOfWeekTest =
    test "dayOfWeek" <|
        \() ->
            DateTime.fromParts 2018 Time.Sep 12 0 0
                |> DateTime.dayOfWeek
                |> Expect.equal Time.Wed


validateTest : Test
validateTest =
    describe "validate" <|
        let
            testMaxDayInMonth year month lastDay =
                describe ("max day in month: " ++ Debug.toString ( year, month ))
                    [ test ("day " ++ String.fromInt lastDay ++ " is valid") <|
                        \() ->
                            DateTime.fromParts year month lastDay 0 0
                                |> DateTime.validate
                                |> Expect.equal (Just <| DateTime.fromParts year month lastDay 0 0)
                    , test ("day " ++ String.fromInt (lastDay + 1) ++ " is not valid") <|
                        \() ->
                            DateTime.fromParts year month (lastDay + 1) 0 0
                                |> DateTime.validate
                                |> Expect.equal Nothing
                    ]
        in
        [ testMaxDayInMonth 2019 Time.Jan 31
        , describe "leap years"
            [ testMaxDayInMonth 2019 Time.Feb 28 -- not a leap year
            , testMaxDayInMonth 2020 Time.Feb 29 -- leap year
            , testMaxDayInMonth 2100 Time.Feb 28 -- not a leap year
            , testMaxDayInMonth 2000 Time.Feb 29 -- leap year
            ]
        , testMaxDayInMonth 2019 Time.Mar 31
        , testMaxDayInMonth 2019 Time.Apr 30
        , testMaxDayInMonth 2019 Time.May 31
        , testMaxDayInMonth 2019 Time.Jun 30
        , testMaxDayInMonth 2019 Time.Jul 31
        , testMaxDayInMonth 2019 Time.Aug 31
        , testMaxDayInMonth 2019 Time.Sep 30
        , testMaxDayInMonth 2019 Time.Oct 31
        , testMaxDayInMonth 2019 Time.Nov 30
        , testMaxDayInMonth 2019 Time.Dec 31
        ]
