module DatePickerTests exposing (all)

import DateTimePicker.DateTime as DateTime
import Expect
import Json.Encode
import Test exposing (..)
import Test.Html.Selector exposing (..)
import TestHelper exposing (clickDate, init, open, selection, simulate, withConfig)
import Time


now : DateTime.DateTime
now =
    DateTime.fromParts 2017 Time.Aug 11 22 30


date : ( Int, Time.Month, Int ) -> DateTime.DateTime
date ( year, month, day ) =
    DateTime.fromParts year month day 0 0


all : Test
all =
    describe "date picker"
        [ let
            allowed config d =
                test ("can select " ++ Debug.toString d ++ " with earliestDate=" ++ Debug.toString config) <|
                    \() ->
                        init now
                            |> withConfig (\c -> { c | earliestDate = config })
                            |> open
                            |> clickDate d
                            |> selection
                            |> Expect.equal (Just (date d))

            notAllowed config d =
                test ("cannot select " ++ Debug.toString d ++ " with earliestDate=" ++ Debug.toString config) <|
                    \() ->
                        init now
                            |> withConfig (\c -> { c | earliestDate = config })
                            |> open
                            |> clickDate d
                            |> selection
                            |> Expect.equal Nothing
          in
          describe "allowable date range"
            [ describe "with no restriction, all dates are allowed"
                [ allowed Nothing ( 2017, Time.Aug, 10 )
                , allowed Nothing ( 2017, Time.Aug, 11 )
                , allowed Nothing ( 2017, Time.Aug, 12 )
                ]
            , describe "with earliestDate, dates including and after are allowed"
                [ notAllowed (Just now) ( 2017, Time.Aug, 10 )
                , allowed (Just now) ( 2017, Time.Aug, 11 )
                , allowed (Just now) ( 2017, Time.Aug, 12 )
                ]
            ]
        , describe "handles leap year"
            [ test "leap day can be selected" <|
                \() ->
                    init (DateTime.fromParts 2020 Time.Feb 26 12 12)
                        |> open
                        |> clickDate ( 2020, Time.Feb, 29 )
                        |> simulate ( "blur", Json.Encode.object [ ( "target", Json.Encode.object [ ( "value", Json.Encode.string "02/29/2020" ) ] ) ] ) [ tag "input" ]
                        |> selection
                        |> Expect.equal (Just (date ( 2020, Time.Feb, 29 )))
            ]
        ]
