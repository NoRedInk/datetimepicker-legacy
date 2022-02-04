module DateTimePicker.Parser exposing (..)

{-|


# THIS IS A HEAVILY MODIFIED FORK OF <https://github.com/abadi199/datetimepicker>

-}

import Char
import DateTimePicker.DateTime as DateTime
import Parser exposing ((|.), (|=), Parser)


parseDate : String -> Maybe DateTime.DateTime
parseDate input =
    let
        validater datetime =
            case DateTime.validate datetime of
                Just validatedDateTime ->
                    Parser.succeed validatedDateTime

                Nothing ->
                    Parser.problem "Invalid date"
    in
    runWithSurroundingSpaceAndValidation dateParser validater input


skipOptionalSpaces : Parser ()
skipOptionalSpaces =
    Parser.chompWhile (\c -> c == ' ')
        |> Parser.getChompedString
        |> Parser.map (\_ -> ())


runWithSurroundingSpaceAndValidation : Parser a -> (a -> Parser b) -> String -> Maybe b
runWithSurroundingSpaceAndValidation innerParser validate input =
    let
        finalParser =
            Parser.succeed identity
                |. skipOptionalSpaces
                |= innerParser
                |. skipOptionalSpaces
                |. Parser.end
                |> Parser.andThen validate
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
