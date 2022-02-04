module Demo exposing (main)

import Browser
import Css
import Css.Media
import DateTimePicker
import DateTimePicker.Config exposing (Config, DatePickerConfig, defaultDatePickerConfig, defaultTimePickerConfig)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Sort exposing (Sorter)
import Sort.Dict as Dict exposing (Dict)
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
    | TimePicker


allPickers : List DemoPicker
allPickers =
    [ DatePicker
    , TimePicker
    ]


demoPickerToString : DemoPicker -> String
demoPickerToString picker =
    case picker of
        DatePicker ->
            "Date picker"

        TimePicker ->
            "Time picker"


demoPickerSorter : Sorter DemoPicker
demoPickerSorter =
    Sort.by demoPickerToString Sort.alphabetical


type alias Model =
    { dates : Dict DemoPicker DateTimePicker.DateTime
    , datePickerState : Dict DemoPicker DateTimePicker.State
    , now : DateTimePicker.DateTime
    }


init : ( Model, Cmd Msg )
init =
    ( { dates = Dict.empty demoPickerSorter
      , datePickerState = Dict.empty demoPickerSorter
      , now = { year = 2022, month = Time.Feb, day = 7, hour = 4, minute = 49 }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


viewPicker : DemoPicker -> DateTimePicker.DateTime -> Maybe DateTimePicker.DateTime -> DateTimePicker.State -> Html Msg
viewPicker which now date state =
    let
        pickerName =
            demoPickerToString which
    in
    case which of
        DatePicker ->
            DateTimePicker.datePickerWithConfig pickerName
                (defaultDatePickerConfig (DatePickerChanged which))
                []
                state
                date

        TimePicker ->
            DateTimePicker.timePickerWithConfig pickerName
                (defaultTimePickerConfig (DatePickerChanged TimePicker))
                []
                state
                date


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
        (List.map (viewPickerSection model) allPickers)


viewPickerSection : Model -> DemoPicker -> Html Msg
viewPickerSection model which =
    Container.view
        [ Container.html
            [ Heading.h1 [ Heading.style Heading.Subhead ] [ text (demoPickerToString which) ]
            , p [] [ text <| Debug.toString <| Dict.get which model.dates ]
            , viewPicker which
                model.now
                (Dict.get which model.dates)
                (Dict.get which model.datePickerState
                    |> Maybe.withDefault (DateTimePicker.initialStateWithToday model.now)
                )
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
                            Dict.remove which model.dates

                        Just date ->
                            Dict.insert which date model.dates
                , datePickerState = Dict.insert which state model.datePickerState
              }
            , Cmd.none
            )
