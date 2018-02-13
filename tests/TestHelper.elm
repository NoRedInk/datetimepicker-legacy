module TestHelper exposing (TestResult, init, open, render, selection, simulate, withConfig)

{-| This module provides functions that allow high-level test interactions with datetimepickers
-}

import Date exposing (Date)
import DateTimePicker
import DateTimePicker.Config exposing (Config, DatePickerConfig, defaultDatePickerConfig)
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Json.Encode as Json
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


{-| The state of a datetimepicker
-}
type TestResult
    = TestResult
        { config : Config (DatePickerConfig {}) ( DateTimePicker.State, Maybe Date )
        , state : DateTimePicker.State
        , date : Maybe Date
        }


{-| Initialize a new DateTimePicker with no initial date selected.

  - `now`: the simulated current time in the test scenario

-}
init : Date -> TestResult
init now =
    TestResult
        { config = defaultDatePickerConfig (,)
        , state = DateTimePicker.initialStateWithToday now
        , date = Nothing
        }


{-| Change the DateTimePicker Config.

NOTE: You must not alter the `onChange` field of the config.

-}
withConfig : (Config (DatePickerConfig {}) ( DateTimePicker.State, Maybe Date ) -> Config (DatePickerConfig {}) ( DateTimePicker.State, Maybe Date )) -> TestResult -> TestResult
withConfig fn (TestResult t) =
    let
        newConfig =
            fn t.config
    in
    TestResult { t | config = newConfig }


{-| Get the currently selected date
-}
selection : TestResult -> Maybe Date
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
        origConfig =
            t.config

        makeResult state date =
            TestResult
                { t
                    | state = state
                    , date = date
                }

        config =
            { origConfig
                | onChange = makeResult
                , attributes = List.map (Attr.map (uncurry makeResult)) origConfig.attributes
            }
    in
    DateTimePicker.datePickerWithConfig
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
                        if String.contains ("The event " ++ Tuple.first event ++ " does not exist on the found node.") message then
                            TestResult t
                        else
                            Debug.crash message

                    Ok result ->
                        result
           )
