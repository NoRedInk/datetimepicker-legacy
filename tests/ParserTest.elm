module ParserTest exposing (..)

import Date
import DateTimePicker.DateTime as DateTime
import DateTimePicker.Parser as Parser
import Expect
import Test exposing (..)


parseTest : Test
parseTest =
    test "it parsers a string correctly" <|
        \() ->
            Parser.parse "9/7/2018 1:07 PM"
                |> Expect.equal (Just (DateTime.fromParts 2018 Date.Sep 7 13 7))
