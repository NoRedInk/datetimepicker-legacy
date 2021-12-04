module FormatterAndParserTests exposing (..)

import DateTimePicker.DateTime as DateTime
import DateTimePicker.Formatter as Formatter
import DateTimePicker.Parser as Parser
import Expect
import Test exposing (..)
import Time


date : DateTime.DateTime
date =
    DateTime.fromParts 2018 Time.Sep 10 13 15


titleFormatterTest : Test
titleFormatterTest =
    test "titleFormatter" <|
        \() ->
            date
                |> Formatter.titleFormatter
                |> Expect.equal "September 2018"


accessibilityDateFormatterTest : Test
accessibilityDateFormatterTest =
    test "accessibilityDateFormatter" <|
        \() ->
            date
                |> Formatter.accessibilityDateFormatter
                |> Expect.equal "10, Monday September 2018"


dateFormatterTest : Test
dateFormatterTest =
    describe "dateFormatter"
        [ test "formats" <|
            \() ->
                date
                    |> Formatter.dateFormatter
                    |> Expect.equal "09/10/2018"
        ]


footerFormatterTest : Test
footerFormatterTest =
    test "footerFormatter" <|
        \() ->
            date
                |> Formatter.footerFormatter
                |> Expect.equal "Monday, September 10, 2018"


dateTimeFormatterTest : Test
dateTimeFormatterTest =
    test "dateTimeFormatter" <|
        \() ->
            date
                |> Formatter.dateTimeFormatter
                |> Expect.equal "09/10/2018 01:15 p.m."


timeFormatterTest : Test
timeFormatterTest =
    test "timeFormatter" <|
        \() ->
            date
                |> Formatter.timeFormatter
                |> Expect.equal "01:15 p.m."


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
