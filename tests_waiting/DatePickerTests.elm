module DatePickerTests exposing (all)

import Date
import DateTimePicker.DateTime as DateTime
import DateTimePicker.Formatter exposing (accessibilityDateFormatter)
import Expect
import Html.Attributes
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Selector exposing (..)
import TestHelper exposing (init, open, render, selection, simulate, withConfig)


now : DateTime.DateTime
now =
    DateTime.fromParts 2017 Date.Aug 11 22 30


all : Test
all =
    describe "date picker"
        [ let
            date ( year, month, day ) =
                DateTime.fromParts year month day 0 0

            allowed config d =
                test ("can select " ++ toString d ++ " with earliestDate=" ++ toString config) <|
                    \() ->
                        init now
                            |> withConfig (\c -> { c | earliestDate = config })
                            |> open
                            |> simulate Event.mouseDown
                                [ tag "td", attribute "aria-label" (accessibilityDateFormatter (date d)) ]
                            |> selection
                            |> Expect.equal (Just (date d))

            notAllowed config d =
                test ("cannot select " ++ toString d ++ " with earliestDate=" ++ toString config) <|
                    \() ->
                        init now
                            |> withConfig (\c -> { c | earliestDate = config })
                            |> open
                            |> simulate Event.mouseDown
                                [ tag "td", attribute "aria-label" (accessibilityDateFormatter (date d)) ]
                            |> selection
                            |> Expect.equal Nothing
          in
          describe "allowable date range"
            [ describe "with no restriction, all dates are allowed"
                [ allowed Nothing ( 2017, Date.Aug, 10 )
                , allowed Nothing ( 2017, Date.Aug, 11 )
                , allowed Nothing ( 2017, Date.Aug, 12 )
                ]
            , describe "with earliestDate, dates including and after are allowed"
                [ notAllowed (Just now) ( 2017, Date.Aug, 10 )
                , allowed (Just now) ( 2017, Date.Aug, 11 )
                , allowed (Just now) ( 2017, Date.Aug, 12 )
                ]
            ]
        ]


attribute : String -> String -> Selector
attribute attr value =
    Html.Attributes.attribute attr value
        |> Test.Html.Selector.attribute
