module RandomDate exposing (main)

import Date
import DateTimePicker
import DateTimePicker.Config exposing (defaultDatePickerConfig, defaultDateTimePickerConfig)
import Html.Styled as Html exposing (Html, button, div, form, label, li, p, text, ul)
import Html.Styled.Attributes exposing (type_)
import Html.Styled.Events exposing (onClick)
import Random


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { dateValue : Maybe DateTimePicker.DateTime
    , datePickerState : DateTimePicker.State
    }


init : ( Model, Cmd Msg )
init =
    ( { dateValue = Nothing
      , datePickerState = DateTimePicker.initialState
      }
    , Cmd.batch
        [ DateTimePicker.initialCmd DateChanged DateTimePicker.initialState
        ]
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


intToMonth : Int -> Date.Month
intToMonth month =
    case month of
        1 ->
            Date.Jan

        2 ->
            Date.Feb

        3 ->
            Date.Mar

        4 ->
            Date.Apr

        5 ->
            Date.May

        6 ->
            Date.Jun

        7 ->
            Date.Jul

        8 ->
            Date.Aug

        9 ->
            Date.Sep

        10 ->
            Date.Oct

        11 ->
            Date.Nov

        _ ->
            Date.Dec
