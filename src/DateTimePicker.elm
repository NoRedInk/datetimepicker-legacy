module DateTimePicker exposing (..)

{-| Shared helpers for DatePicker and TimePicker.


# This is a heavily-modified version of <https://github.com/abadi199/datetimepicker/blob/master/src/DateTimePicker/Svg.elm>

-}

import DateTimePicker.DateTime as DateTime
import DateTimePicker.DateUtils
import DateTimePicker.Internal exposing (InternalState(..), StateValue, TimeSelection, initialStateValue)
import Html.Styled exposing (Html)
import Nri.Ui.TextInput.V7 as TextInput


type alias State =
    InternalState


type alias DateTime =
    DateTime.DateTime



-- VIEWS


viewInput :
    String
    -> List (TextInput.Attribute String msg)
    ->
        { config
            | onChange : State -> Maybe DateTime.DateTime -> msg
            , fromInput : String -> Maybe DateTime.DateTime
            , toInput : DateTime.DateTime -> String
        }
    -> StateValue
    -> Maybe DateTime.DateTime
    -> Html msg
viewInput label attributes config stateValue currentDate =
    TextInput.view label
        ([ TextInput.onFocus (datePickerFocused config stateValue currentDate)
         , TextInput.onBlur (blurInputHandler config stateValue currentDate)
         , TextInput.onEnter (blurInputHandler config stateValue currentDate)
         , TextInput.text
            (\newValue ->
                config.onChange (setTextInput newValue stateValue)
                    currentDate
            )
         , TextInput.value stateValue.textInputValue
         ]
            ++ attributes
        )



-- EVENT HANDLERS


blurInputHandler :
    { config
        | onChange : State -> Maybe DateTime.DateTime -> msg
        , fromInput : String -> Maybe DateTime.DateTime
        , toInput : DateTime.DateTime -> String
    }
    -> StateValue
    -> Maybe DateTime.DateTime
    -> msg
blurInputHandler config stateValue currentDate =
    case config.fromInput stateValue.textInputValue of
        Just date ->
            let
                updatedValue =
                    { stateValue
                        | date = Just date
                        , inputFocused = False
                    }
            in
            config.onChange (updateTextInputFromDate config updatedValue) (Just date)

        Nothing ->
            let
                updatedDate =
                    case currentDate of
                        Just _ ->
                            Nothing

                        Nothing ->
                            stateValue.date

                updatedValue =
                    { stateValue
                        | date = updatedDate
                        , hourPickerStart = initialStateValue.hourPickerStart
                        , minutePickerStart = initialStateValue.minutePickerStart
                        , inputFocused = False
                    }
            in
            config.onChange (setTextInput "" updatedValue) Nothing


datePickerFocused :
    { config
        | onChange : State -> Maybe DateTime.DateTime -> msg
        , toInput : DateTime.DateTime -> String
    }
    -> StateValue
    -> Maybe DateTime.DateTime
    -> msg
datePickerFocused config stateValue currentDate =
    let
        updatedTitleDate =
            case currentDate of
                Nothing ->
                    stateValue.titleDate

                Just _ ->
                    currentDate
    in
    config.onChange
        (updateTextInputFromDate config
            { stateValue
                | inputFocused = True
                , titleDate = updatedTitleDate
                , date = currentDate
            }
        )
        currentDate


setTextInput : String -> StateValue -> State
setTextInput value state =
    InternalState { state | textInputValue = value }


updateTextInputFromDate :
    { config | toInput : DateTime.DateTime -> String }
    -> StateValue
    -> State
updateTextInputFromDate config state =
    InternalState
        { state
            | textInputValue =
                Maybe.map config.toInput state.date
                    |> Maybe.withDefault state.textInputValue
        }
