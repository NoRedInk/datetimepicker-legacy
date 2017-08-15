module DatePickerTests exposing (all)

import Date exposing (Date)
import Expect
import Html.Attributes
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Selector exposing (..)
import TestHelper exposing (init, open, render, selection, simulate, withConfig)


now : Date
now =
    -- 2017-08-11T22:30:55Z
    Date.fromTime 1502490656000


all : Test
all =
    describe "date picker"
        [ describe "allowable date range"
            [ test "can select a valid date" <|
                \() ->
                    init now
                        |> withConfig (\c -> { c | earliestDate = Nothing })
                        |> open
                        |> simulate Event.mouseDown
                            [ tag "td", attribute "aria-label" "15, Tuesday August 2017" ]
                        |> selection
                        |> toString
                        |> Expect.equal "Just <Tue Aug 15 2017 00:00:00 GMT-0500 (CDT)>"
            , test "does not select an invalid date" <|
                \() ->
                    init now
                        |> withConfig (\c -> { c | earliestDate = Just now })
                        |> open
                        |> simulate Event.mouseDown
                            [ tag "td", attribute "aria-label" "10, Thursday August 2017" ]
                        |> selection
                        |> toString
                        |> Expect.equal "Nothing"
            ]
        ]


attribute : String -> String -> Selector
attribute attr value =
    Test.Html.Selector.attribute <| Html.Attributes.attribute attr value
