module DateTimePicker.Parser exposing
    ( parseDate
    , parseDateTime
    , parseTime
    )

{-|


# THIS IS A HEAVILY MODIFIED FORK OF <https://github.com/abadi199/datetimepicker>

-}

import Char
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
    Parser.chompWhile (\c -> c == ' ')
        |> Parser.getChompedString
        |> Parser.map (\_ -> ())


skipAtLeastOneSpace : Parser ()
skipAtLeastOneSpace =
    Parser.succeed ()
        |. Parser.chompIf (\c -> c == ' ')
        |. Parser.chompWhile (\c -> c == ' ')
        |> Parser.getChompedString
        |> Parser.map (\_ -> ())


amPm : Parser String
amPm =
    Parser.oneOf
        [ Parser.map (\_ -> "AM") <|
            Parser.oneOf
                [ Parser.symbol "AM"
                , Parser.symbol "am"
                , Parser.symbol "aM"
                , Parser.symbol "Am"
                , Parser.symbol "a.m."
                , Parser.symbol "A.M."
                ]
        , Parser.map (\_ -> "PM") <|
            Parser.oneOf
                [ Parser.symbol "PM"
                , Parser.symbol "pm"
                , Parser.symbol "pM"
                , Parser.symbol "Pm"
                , Parser.symbol "p.m."
                , Parser.symbol "P.M."
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
                                Parser.problem "Invalid date"
                    )
    in
    Parser.run finalParser input
        |> Result.toMaybe


{-| A fixed-length integer padded with zeroes.
-}
looseInt : Parser Int
looseInt =
    Parser.succeed ()
        |. Parser.chompIf Char.isDigit
        |. Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen
            (\digitsString ->
                case String.toInt digitsString of
                    Just int ->
                        Parser.succeed int

                    Nothing ->
                        Parser.problem (digitsString ++ " is not an integer")
            )


clamped : Int -> Int -> Parser Int -> Parser Int
clamped min max previousParser =
    Parser.andThen
        (\int ->
            if int > max || int < min then
                Parser.problem "Int out of range"

            else
                Parser.succeed int
        )
        previousParser


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
