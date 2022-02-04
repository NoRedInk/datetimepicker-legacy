module TimePickerTests exposing (..)

import Expect
import Test exposing (..)
import Time
import TimePicker exposing (Time)


tests : Test
tests =
    describe "TimePicker"
        ([ List.map fromStringTest
            [ ( "01:15 p.m.", { hour = 13, minute = 15 } )
            , ( "1:15 p.m.", { hour = 13, minute = 15 } )
            , ( "1:15p.m.", { hour = 13, minute = 15 } )
            , ( "1:15pm", { hour = 13, minute = 15 } )
            , ( "1:15PM", { hour = 13, minute = 15 } )
            , ( "1:15 PM", { hour = 13, minute = 15 } )
            , ( "12:00 a.m.", { hour = 0, minute = 0 } )
            ]
         , List.map toStringTest
            [ ( { hour = 13, minute = 15 }, "01:15 p.m." )
            , ( { hour = 0, minute = 0 }, "12:00 a.m." )
            ]
         ]
            |> List.concat
        )


fromStringTest : ( String, Time ) -> Test
fromStringTest ( startingString, expectedValue ) =
    test ("fromString " ++ startingString) <|
        \() ->
            Expect.equal (Just expectedValue) (TimePicker.fromString startingString)


toStringTest : ( Time, String ) -> Test
toStringTest ( startingTime, expectedValue ) =
    test ("toString " ++ Debug.toString startingTime) <|
        \() ->
            Expect.equal expectedValue (TimePicker.toString startingTime)
