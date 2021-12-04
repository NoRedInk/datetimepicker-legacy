module FormatterAndParserTests exposing (..)

import DateTimePicker.DateTime as DateTime
import DateTimePicker.Formatter as Formatter exposing (dateFormatter, dateTimeFormatter, timeFormatter)
import DateTimePicker.Parser as Parser exposing (parseDate, parseDateTime, parseTime)
import Expect
import Test exposing (..)
import Time


formatParsesTo :
    String
    -> (DateTime.DateTime -> String)
    -> (String -> Maybe DateTime.DateTime)
    -> List ( String, DateTime.DateTime, DateTime.DateTime )
    -> List Test
formatParsesTo formatName format parse examples =
    List.concatMap
        (\( expectedStr, date_, finalDate ) ->
            [ test ("Format " ++ formatName ++ " follows expected format for " ++ expectedStr) <|
                \() ->
                    Expect.equal expectedStr (format date_)
            , test ("Format " ++ formatName ++ " can be parsed for " ++ expectedStr) <|
                \() ->
                    parse (format date_)
                        |> Expect.equal (Just finalDate)
            ]
        )
        examples


date : DateTime.DateTime
date =
    DateTime.fromParts 2018 Time.Sep 10 13 15


tests : Test
tests =
    [ formatParsesTo "dateFormatter" dateFormatter parseDate <|
        [ ( "01/01/2018"
          , DateTime.fromParts 2018 Time.Jan 1 10 15
          , DateTime.fromParts 2018 Time.Jan 1 0 0
          )
        , ( "09/10/2018"
          , DateTime.fromParts 2018 Time.Sep 10 1 3
          , DateTime.fromParts 2018 Time.Sep 10 0 0
          )
        ]
    , formatParsesTo "timeFormatter" timeFormatter parseTime <|
        [ ( "01:15 p.m."
          , DateTime.fromParts 2018 Time.Sep 10 13 15
          , DateTime.fromParts 0 Time.Jan 1 13 15
          )
        , ( "12:00 a.m."
          , DateTime.fromParts 2018 Time.Sep 10 0 0
          , DateTime.fromParts 0 Time.Jan 1 0 0
          )
        ]
    , formatParsesTo "dateTimeFormatter" dateTimeFormatter parseDateTime <|
        [ ( "09/10/2018 01:15 p.m."
          , DateTime.fromParts 2018 Time.Sep 10 13 15
          , DateTime.fromParts 2018 Time.Sep 10 13 15
          )
        , ( "01/10/2018 12:00 a.m."
          , DateTime.fromParts 2018 Time.Jan 10 0 0
          , DateTime.fromParts 2018 Time.Jan 10 0 0
          )
        ]
    ]
        |> List.concat
        |> describe "Formatters and Parsers work"


accessibilityDateFormatterTest : Test
accessibilityDateFormatterTest =
    test "accessibilityDateFormatter" <|
        \() ->
            date
                |> Formatter.accessibilityDateFormatter
                |> Expect.equal "10, Monday September 2018"


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
