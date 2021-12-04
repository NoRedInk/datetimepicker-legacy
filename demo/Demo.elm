module Demo exposing (main)

import Browser
import Css
import DateTimePicker
import DateTimePicker.Config exposing (Config, DatePickerConfig, defaultDatePickerConfig, defaultDateTimePickerConfig, defaultTimePickerConfig)
import Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V2 as Heading
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
    | DateTimePicker
    | TimePicker


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


viewPicker : DemoPicker -> DateTimePicker.DateTime -> Maybe DateTimePicker.DateTime -> DateTimePicker.State -> Html Msg
viewPicker which now date state =
    case which of
        DatePicker ->
            DateTimePicker.datePickerWithConfig "Date Picker"
                (defaultDatePickerConfig (DatePickerChanged which))
                []
                state
                date

        DateTimePicker ->
            DateTimePicker.dateTimePickerWithConfig "Date and Time Picker"
                (defaultDateTimePickerConfig (DatePickerChanged DateTimePicker))
                []
                state
                date

        TimePicker ->
            DateTimePicker.timePickerWithConfig "Time Picker"
                (defaultTimePickerConfig (DatePickerChanged TimePicker))
                []
                state
                date


view : Model -> Html Msg
view model =
    let
        allPickers =
            [ DatePicker
            , DateTimePicker
            , TimePicker
            ]
    in
    div
        [ css
            [ Css.displayFlex
            , Css.property "gap" "20px"
            ]
        ]
        (List.map (viewPickerSection model) allPickers)


viewPickerSection : Model -> DemoPicker -> Html Msg
viewPickerSection model which =
    Container.view
        [ Container.html
            [ Heading.h1 [ Heading.style Heading.Small ] [ text (Debug.toString which) ]
            , div [ css [ Css.displayFlex ] ]
                [ viewPicker which
                    model.now
                    (Dict.get (Debug.toString which) model.dates)
                    (Dict.get (Debug.toString which) model.datePickerState
                        |> Maybe.withDefault (DateTimePicker.initialStateWithToday model.now)
                    )
                , text <| Debug.toString <| Dict.get (Debug.toString which) model.dates
                ]
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
