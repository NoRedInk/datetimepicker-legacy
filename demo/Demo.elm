module Demo exposing (main)

import Browser
import Css
import Css.Media
import DatePicker
import DateTimePicker
import DateTimePicker.Config exposing (DatePickerConfig, defaultDatePickerConfig, defaultTimePickerConfig)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V2 as Heading
import Time
import TimePicker


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = Html.toUnstyled << view
        , subscriptions = subscriptions
        }


type alias Model =
    { date : Maybe DateTimePicker.DateTime
    , datePickerState : DateTimePicker.State
    , time : Maybe DateTimePicker.DateTime
    , timePickerState : DateTimePicker.State
    }


init : ( Model, Cmd Msg )
init =
    let
        now : DateTimePicker.DateTime
        now =
            { year = 2022
            , month = Time.Feb
            , day = 7
            , hour = 4
            , minute = 49
            }
    in
    ( { date = Nothing
      , datePickerState = DatePicker.init now
      , time = Nothing
      , timePickerState = TimePicker.init now
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    main_
        [ css
            [ Css.displayFlex
            , Css.property "gap" "20px"
            , Css.flexDirection Css.column
            , Css.margin Css.auto
            , Css.maxWidth (Css.px 1000)
            ]
        ]
        [ Container.view
            [ Container.html
                [ Heading.h1 [ Heading.style Heading.Subhead ] [ text "Date picker" ]
                , p [] [ text <| Debug.toString model.date ]
                , DatePicker.view "Date picker"
                    (defaultDatePickerConfig DatePickerChanged)
                    []
                    model.datePickerState
                    model.date
                ]
            ]
        , Container.view
            [ Container.html
                [ Heading.h1 [ Heading.style Heading.Subhead ] [ text "Time picker" ]
                , p [] [ text <| Debug.toString model.time ]
                , TimePicker.view "Time picker"
                    (defaultTimePickerConfig TimePickerChanged)
                    []
                    model.timePickerState
                    model.time
                ]
            ]
        ]


type Msg
    = DatePickerChanged DateTimePicker.State (Maybe DateTimePicker.DateTime)
    | TimePickerChanged DateTimePicker.State (Maybe DateTimePicker.DateTime)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DatePickerChanged state value ->
            ( { model | date = value, datePickerState = state }
            , Cmd.none
            )

        TimePickerChanged state value ->
            ( { model | time = value, timePickerState = state }
            , Cmd.none
            )
