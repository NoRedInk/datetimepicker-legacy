module DatePickerTests exposing (all)

import Date exposing (Date)
import Date.Extra.Core
import Date.Extra.Create
import DateTimePicker.Formatter exposing (accessibilityDateFormatter)
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
        [ let
            date ( year, month, day ) =
                Date.Extra.Create.dateFromFields year (Date.Extra.Core.intToMonth month) day 0 0 0 0

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
                [ allowed Nothing ( 2017, 8, 10 )
                , allowed Nothing ( 2017, 8, 11 )
                , allowed Nothing ( 2017, 8, 12 )
                ]
            , describe "with earliestDate, dates including and after are allowed"
                [ notAllowed (Just now) ( 2017, 8, 10 )
                , allowed (Just now) ( 2017, 8, 11 )
                , allowed (Just now) ( 2017, 8, 12 )
                ]
            ]
        ]


attribute : String -> String -> Selector
attribute attr value =
    Test.Html.Selector.attribute <| Html.Attributes.attribute attr value
