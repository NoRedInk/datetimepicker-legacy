module RandomDate exposing (main)

import Browser
import DateTimePicker
import DateTimePicker.Config exposing (defaultDatePickerConfig, defaultDateTimePickerConfig)
import Html.Styled as Html exposing (Html, button, div, form, label, li, p, text, ul)
import Html.Styled.Attributes exposing (type_)
import Html.Styled.Events exposing (onClick)
import Random
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view >> Html.toUnstyled
        , subscriptions = subscriptions
        }


type alias Model =
    { dateValue : Maybe DateTimePicker.DateTime
    , datePickerState : DateTimePicker.State
    }


init : ( Model, Cmd Msg )
init =
    ( { dateValue = Nothing
      , datePickerState = DateTimePicker.initialStateWithToday { year = 2018, month = Time.Nov, day = 13, hour = 14, minute = 0 }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        analogDateTimePickerConfig =
            let
                defaultDateTimeConfig =
                    defaultDateTimePickerConfig DateChanged
            in
            { defaultDateTimeConfig
                | timePickerType = DateTimePicker.Config.Digital
                , allowYearNavigation = False
            }
    in
    form []
        [ div []
            [ p
                []
                [ label []
                    [ text "Date Picker: "
                    , DateTimePicker.dateTimePickerWithConfig
                        analogDateTimePickerConfig
                        []
                        model.datePickerState
                        model.dateValue
                    ]
                ]
            ]
        , button [ type_ "button", onClick GetRandomDate ] [ text "Random Date" ]
        ]


type Msg
    = NoOp
    | DateChanged DateTimePicker.State (Maybe DateTimePicker.DateTime)
    | GetRandomDate
    | GetRandomDateCompleted (Maybe DateTimePicker.DateTime)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DateChanged state value ->
            ( { model | dateValue = value, datePickerState = state }, Cmd.none )

        GetRandomDate ->
            ( model, getRandomDate )

        GetRandomDateCompleted randomDate ->
            ( { model | dateValue = randomDate }, Cmd.none )


getRandomDate : Cmd Msg
getRandomDate =
    let
        dateGenerator =
            Random.map5 DateTimePicker.dateTime
                (Random.int 2015 2018)
                (Random.map intToMonth (Random.int 1 12))
                (Random.int 1 28)
                (Random.int 0 23)
                (Random.int 0 59)
                |> Random.map Just
    in
    Random.generate GetRandomDateCompleted dateGenerator


intToMonth : Int -> Time.Month
intToMonth month =
    case month of
        1 ->
            Time.Jan

        2 ->
            Time.Feb

        3 ->
            Time.Mar

        4 ->
            Time.Apr

        5 ->
            Time.May

        6 ->
            Time.Jun

        7 ->
            Time.Jul

        8 ->
            Time.Aug

        9 ->
            Time.Sep

        10 ->
            Time.Oct

        11 ->
            Time.Nov

        _ ->
            Time.Dec
