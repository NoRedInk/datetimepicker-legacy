module DateTimePicker.Parser
    exposing
        ( parseDate
        , parseDateTime
        , parseTime
        )

import Char
import Date
import DateTimePicker.DateTime as DateTime
import Parser exposing ((|.), (|=), Parser)


parseDate : String -> Maybe DateTime.DateTime
parseDate input =
    runWithSurroundingSpaceAndValidation dateParser input


parseTime : String -> Maybe DateTime.DateTime
parseTime input =
    runWithSurroundingSpaceAndValidation timeParser input


parseDateTime : String -> Maybe DateTime.DateTime
parseDateTime input =
    runWithSurroundingSpaceAndValidation dateTimeParser input


skipOptionalSpaces : Parser ()
skipOptionalSpaces =
    Parser.ignore Parser.zeroOrMore (\c -> c == ' ')


skipAtLeastOneSpace : Parser ()
skipAtLeastOneSpace =
    Parser.ignore Parser.oneOrMore (\c -> c == ' ')


amPm : Parser String
amPm =
    Parser.oneOf
        [ Parser.map (\_ -> "AM") <|
            Parser.oneOf
                [ Parser.symbol "AM"
                , Parser.symbol "am"
                , Parser.symbol "aM"
                , Parser.symbol "Am"
                ]
        , Parser.map (\_ -> "PM") <|
            Parser.oneOf
                [ Parser.symbol "PM"
                , Parser.symbol "pm"
                , Parser.symbol "pM"
                , Parser.symbol "Pm"
                ]
        ]


runWithSurroundingSpaceAndValidation : Parser DateTime.DateTime -> String -> Maybe DateTime.DateTime
runWithSurroundingSpaceAndValidation innerParser input =
    let
        finalParser =
            Parser.succeed identity
                |. skipOptionalSpaces
                |= innerParser
                |. skipOptionalSpaces
                |. Parser.end
                |> Parser.andThen
                    (\datetime ->
                        case DateTime.validate datetime of
                            Just validatedDateTime ->
                                Parser.succeed validatedDateTime

                            Nothing ->
                                Parser.fail "Invalid date"
                    )
    in
    Parser.run finalParser input
        |> Result.toMaybe


{-| A fixed-length integer padded with zeroes.
-}
looseInt : Parser Int
looseInt =
    Parser.keep Parser.oneOrMore (\c -> Char.isDigit c)
        |> Parser.andThen
            (\digitsString ->
                case String.toInt digitsString of
                    Ok int ->
                        Parser.succeed int

                    Err message ->
                        Parser.fail message
            )


clamped : Int -> Int -> Parser Int -> Parser Int
clamped min max previousParser =
    Parser.andThen
        (\int ->
            if int > max || int < min then
                Parser.fail "Int out of range"
            else
                Parser.succeed int
        )
        previousParser


makeDateTime : Date.Month -> Int -> Int -> Int -> Int -> String -> DateTime.DateTime
makeDateTime month day year hour minute amPm =
    DateTime.fromDate year month day
        |> DateTime.setTime hour minute amPm


{-| Parse the exact format "%m/%d/%Y"
-}
dateParser : Parser DateTime.DateTime
dateParser =
    Parser.succeed (\month day year -> DateTime.fromDate year month day)
        |= Parser.map DateTime.intToMonth (clamped 1 12 looseInt)
        |. skipOptionalSpaces
        |. Parser.symbol "/"
        |. skipOptionalSpaces
        |= clamped 1 31 looseInt
        |. skipOptionalSpaces
        |. Parser.symbol "/"
        |. skipOptionalSpaces
        |= Parser.int


{-| Parse the exact format "%I:%M %p"
-}
timeParser : Parser DateTime.DateTime
timeParser =
    Parser.succeed DateTime.fromTime
        |= clamped 1 12 looseInt
        |. skipOptionalSpaces
        |. Parser.symbol ":"
        |. skipOptionalSpaces
        |= clamped 0 59 looseInt
        |. skipAtLeastOneSpace
        |= amPm


dateTimeParser : Parser DateTime.DateTime
dateTimeParser =
    Parser.succeed
        (\dateComponent timeComponent ->
            DateTime.fromParts
                dateComponent.year
                dateComponent.month
                dateComponent.day
                timeComponent.hour
                timeComponent.minute
        )
        |= dateParser
        |. skipAtLeastOneSpace
        |= timeParser
