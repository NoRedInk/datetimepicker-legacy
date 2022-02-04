module TestHelper exposing (TestResult, clickDate, init, open, render, selection, simulate, withConfig)

{-| This module provides functions that allow high-level test interactions with datetimepickers
-}

import DatePicker
import DateTimePicker.Config exposing (DatePickerConfig, defaultDatePickerConfig)
import DateTimePicker.DateTime as DateTime
import DateTimePicker.Formatter exposing (accessibilityDateFormatter)
import Html.Attributes
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Json.Encode as Json
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)
import Time


{-| The state of a datetimepicker
-}
type TestResult
    = TestResult
        { config : DatePickerConfig ( DatePicker.Model, Maybe DateTime.DateTime )
        , state : DatePicker.Model
        , date : Maybe DateTime.DateTime
        }


{-| Initialize a new DateTimePicker with no initial date selected.

  - `now`: the simulated current time in the test scenario

-}
init : DateTime.DateTime -> TestResult
init now =
    TestResult
        { config = defaultDatePickerConfig Tuple.pair
        , state = DatePicker.init now
        , date = Nothing
        }


{-| Change the DateTimePicker Config.

NOTE: You must not alter the `onChange` field of the config.

-}
withConfig :
    (DatePickerConfig ( DatePicker.Model, Maybe DateTime.DateTime )
     -> DatePickerConfig ( DatePicker.Model, Maybe DateTime.DateTime )
    )
    -> TestResult
    -> TestResult
withConfig fn (TestResult t) =
    let
        newConfig =
            fn t.config
    in
    TestResult { t | config = newConfig }


{-| Get the currently selected date
-}
selection : TestResult -> Maybe DateTime.DateTime
selection (TestResult t) =
    t.date


{-| Simulate opening the datetimpicker (by focusing the input field)
-}
open : TestResult -> TestResult
open =
    simulate Event.focus [ tag "input" ]


{-| Render the view of the datetimepicker with the given state,
and return a `Test.Html.Query.Single` of the resulting Html.
-}
render : TestResult -> Query.Single TestResult
render (TestResult t) =
    let
        uncurry f ( a, b ) =
            f a b

        origConfig =
            t.config

        makeResult state date =
            TestResult
                { t
                    | state = state
                    , date = date
                }

        config =
            { usePicker = origConfig.usePicker
            , fromInput = origConfig.fromInput
            , toInput = origConfig.toInput
            , nameOfDays = origConfig.nameOfDays
            , firstDayOfWeek = origConfig.firstDayOfWeek
            , allowYearNavigation = origConfig.allowYearNavigation
            , earliestDate = origConfig.earliestDate
            , attributes = List.map (Attr.map (uncurry makeResult)) origConfig.attributes
            , onChange = makeResult
            }
    in
    DatePicker.view "Label"
        config
        []
        t.state
        t.date
        |> Html.toUnstyled
        |> Query.fromHtml


{-| Simulate a DOM event.

  - If no nodes match the selector, this will crash.
  - If more than one node matches the selector, this will crash.
  - If one node matches but does not handle the given event, this will silently succeed.
    (This matches what would happen, for example, if a user clicked a node with no onClick handler.)

-}
simulate : ( String, Json.Value ) -> List Selector -> TestResult -> TestResult
simulate event selector (TestResult t) =
    render (TestResult t)
        |> Query.find selector
        |> Event.simulate event
        |> Event.toResult
        |> (\r ->
                case r of
                    Err message ->
                        if String.contains ("I found a node, but it does not listen for \"" ++ Tuple.first event ++ "\" events like I expected it would.") message then
                            TestResult t

                        else
                            Debug.todo message

                    Ok result ->
                        result
           )


{-| Simulates clicking on a specific date within the datepicker popup.
-}
clickDate : ( Int, Time.Month, Int ) -> TestResult -> TestResult
clickDate ( year, month, day ) =
    simulate Event.mouseDown
        [ tag "td"
        , Test.Html.Selector.attribute <|
            Html.Attributes.attribute "aria-label" (accessibilityDateFormatter (DateTime.fromParts year month day 0 0))
        ]
