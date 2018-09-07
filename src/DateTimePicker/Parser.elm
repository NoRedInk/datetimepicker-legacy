module DateTimePicker.Parser exposing (..)

import Char
import DateTimePicker.DateTime as DateTime
import Parser exposing ((|.), (|=), Parser)


parse : String -> Maybe DateTime.DateTime
parse input =
    Parser.run parser input
        |> Debug.log "result"
        |> Result.toMaybe


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


makeDateTime : Int -> Int -> Int -> Int -> Int -> String -> DateTime.DateTime
makeDateTime month day year hour minute amPm =
    DateTime.fromDate year month day
        |> DateTime.setTime hour minute amPm


{-| Parse the exact format "%m/%d/%Y %I:%M %p"
-}
parser : Parser DateTime.DateTime
parser =
    -- Write a function to make a datetime
    Parser.succeed makeDateTime
        |. skipOptionalSpaces
        |= looseInt
        |. skipOptionalSpaces
        |. Parser.symbol "/"
        |. skipOptionalSpaces
        |= looseInt
        |. skipOptionalSpaces
        |. Parser.symbol "/"
        |. skipOptionalSpaces
        |= Parser.int
        |. skipAtLeastOneSpace
        |= looseInt
        |. skipOptionalSpaces
        |. Parser.symbol ":"
        |. skipOptionalSpaces
        |= looseInt
        |. skipAtLeastOneSpace
        |= amPm
        |. skipOptionalSpaces
        |. Parser.end
