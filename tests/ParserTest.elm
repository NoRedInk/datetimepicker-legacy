module ParserTest exposing (invalidDateTest, parseDateTest, parseDateTimeTest, parseTimeTest)

import DateTimePicker.DateTime as DateTime
import DateTimePicker.Parser as Parser
import Expect
import Test exposing (..)
import Time


parseDateTest : Test
parseDateTest =
    describe "it parses a date correctly"
        [ test "some date" <|
            \() ->
                Parser.parseDate "9/7/2018"
                    |> Expect.equal (Just (DateTime.fromParts 2018 Time.Sep 7 0 0))
        , test "a leap day" <|
            \() ->
                Parser.parseDate "02/29/2020"
                    |> Expect.equal (Just (DateTime.fromParts 2020 Time.Feb 29 0 0))
        ]


parseTimeTest : Test
parseTimeTest =
    test "it parses a time correctly" <|
        \() ->
            Parser.parseTime "1:15 PM"
                |> Expect.equal (Just (DateTime.fromParts 0 Time.Jan 1 13 15))


parseDateTimeTest : Test
parseDateTimeTest =
    test "it parses a date and time correctly" <|
        \() ->
            Parser.parseDateTime "9/7/2018 1:15 PM"
                |> Expect.equal (Just (DateTime.fromParts 2018 Time.Sep 7 13 15))


invalidDateTest : Test
invalidDateTest =
    test "it fails if a date is out of range" <|
        \() ->
            Parser.parseDateTime "2/30/2018 1:15 PM"
                |> Expect.equal Nothing
