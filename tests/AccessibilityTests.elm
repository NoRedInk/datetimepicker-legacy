module AccessibilityTests exposing (datePickerTests)

import DateTimePicker.DateTime as DateTime
import Html.Attributes
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)
import TestHelper exposing (init, open, render)
import Time


now : DateTime.DateTime
now =
    DateTime.fromParts 2017 Time.Aug 11 22 30


datePickerTests : Test
datePickerTests =
    describe "date picker accessibility"
        [ test "date cells should have role=button" <|
            \() ->
                init now
                    |> open
                    |> render
                    |> Query.findAll [ tag "td" ]
                    |> Query.each
                        (Query.has [ attribute "role" "button" ])
        , test "date cells should have labels" <|
            \() ->
                init now
                    |> open
                    |> render
                    |> Query.has
                        [ tag "td"
                        , attribute "aria-label" "15, Tuesday August 2017"
                        ]
        ]


attribute : String -> String -> Selector
attribute attr value =
    Test.Html.Selector.attribute <| Html.Attributes.attribute attr value
