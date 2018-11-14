module Demo exposing (main)

import Browser
import DateTimePicker
import DateTimePicker.Config exposing (Config, DatePickerConfig, TimePickerConfig, defaultDatePickerConfig, defaultDateTimePickerConfig, defaultTimePickerConfig)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, div, form, h3, label, li, p, text, ul)
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = Html.toUnstyled << view
        , subscriptions = subscriptions
        }


type DemoPicker
    = DatePicker
    | DigitalDateTimePicker
    | AnalogDateTimePicker
    | TimePicker
    | NoPicker
    | LimitedRangePicker


type alias Model =
    { dates : Dict String DateTimePicker.DateTime -- The key is actually a DemoPicker
    , datePickerState : Dict String DateTimePicker.State -- The key is actually a DemoPicker
    , now : DateTimePicker.DateTime
    }


init : ( Model, Cmd Msg )
init =
    ( { dates = Dict.empty
      , datePickerState = Dict.empty
      , now = { year = 2018, month = Time.Sep, day = 7, hour = 4, minute = 49 }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


analogDateTimePickerConfig : Config (DatePickerConfig TimePickerConfig) Msg
analogDateTimePickerConfig =
    let
        defaultDateTimeConfig =
            defaultDateTimePickerConfig (DatePickerChanged AnalogDateTimePicker)
    in
    { defaultDateTimeConfig
        | timePickerType = DateTimePicker.Config.Analog
        , allowYearNavigation = False
    }


timePickerConfig : Config TimePickerConfig Msg
timePickerConfig =
    let
        defaultDateTimeConfig =
            defaultTimePickerConfig (DatePickerChanged TimePicker)
    in
    { defaultDateTimeConfig
        | timePickerType = DateTimePicker.Config.Analog
    }


noPickerConfig : Config (DatePickerConfig {}) Msg
noPickerConfig =
    let
        defaultDateConfig =
            defaultDatePickerConfig (DatePickerChanged NoPicker)
    in
    { defaultDateConfig
        | usePicker = False
        , attributes = []
    }


digitalDateTimePickerConfig : Config (DatePickerConfig TimePickerConfig) Msg
digitalDateTimePickerConfig =
    let
        defaultDateTimeConfig =
            defaultDateTimePickerConfig (DatePickerChanged DigitalDateTimePicker)
    in
    { defaultDateTimeConfig
        | timePickerType = DateTimePicker.Config.Digital
    }


digitalTimePickerConfig : Config TimePickerConfig Msg
digitalTimePickerConfig =
    let
        defaultDateTimeConfig =
            defaultTimePickerConfig (DatePickerChanged TimePicker)
    in
    { defaultDateTimeConfig
        | timePickerType = DateTimePicker.Config.Digital
    }


limitedPickerConfig : DateTimePicker.DateTime -> Config (DatePickerConfig TimePickerConfig) Msg
limitedPickerConfig now =
    let
        defaultDateTimeConfig =
            defaultDateTimePickerConfig (DatePickerChanged LimitedRangePicker)
    in
    { defaultDateTimeConfig
        | timePickerType = DateTimePicker.Config.Digital
        , earliestDate = Just now
    }


viewPicker : DemoPicker -> DateTimePicker.DateTime -> Maybe DateTimePicker.DateTime -> DateTimePicker.State -> Html Msg
viewPicker which now date state =
    p []
        [ label []
            [ text (Debug.toString which)
            , text ":"
            , case which of
                DatePicker ->
                    DateTimePicker.datePicker (DatePickerChanged which) [] state date

                DigitalDateTimePicker ->
                    DateTimePicker.dateTimePickerWithConfig digitalDateTimePickerConfig [] state date

                AnalogDateTimePicker ->
                    DateTimePicker.dateTimePickerWithConfig analogDateTimePickerConfig [] state date

                TimePicker ->
                    DateTimePicker.timePickerWithConfig digitalTimePickerConfig [] state date

                NoPicker ->
                    DateTimePicker.datePickerWithConfig noPickerConfig [] state date

                LimitedRangePicker ->
                    DateTimePicker.dateTimePickerWithConfig (limitedPickerConfig now) [] state date
            ]
        ]


view : Model -> Html Msg
view model =
    let
        allPickers =
            [ DatePicker
            , DigitalDateTimePicker
            , AnalogDateTimePicker
            , TimePicker
            , NoPicker
            , LimitedRangePicker
            ]
    in
    form []
        [ allPickers
            |> List.map
                (\which ->
                    viewPicker which
                        model.now
                        (Dict.get (Debug.toString which) model.dates)
                        (Dict.get (Debug.toString which) model.datePickerState |> Maybe.withDefault (DateTimePicker.initialStateWithToday model.now))
                )
            |> div []
        , h3 [] [ text "Selected values" ]
        , p []
            [ allPickers
                |> List.map
                    (\which ->
                        li [] [ text (Debug.toString which), text ": ", text <| Debug.toString <| Dict.get (Debug.toString which) model.dates ]
                    )
                |> ul []
            ]
        ]


type Msg
    = DatePickerChanged DemoPicker DateTimePicker.State (Maybe DateTimePicker.DateTime)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DatePickerChanged which state value ->
            ( { model
                | dates =
                    case value of
                        Nothing ->
                            Dict.remove (Debug.toString which) model.dates

                        Just date ->
                            Dict.insert (Debug.toString which) date model.dates
                , datePickerState = Dict.insert (Debug.toString which) state model.datePickerState
              }
            , Cmd.none
            )
