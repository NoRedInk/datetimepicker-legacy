module DateTimePicker.Helpers exposing (updateCurrentDate, updateTimeIndicator)

{-|


# THIS IS A HEAVILY MODIFIED FORK OF <https://github.com/abadi199/datetimepicker>

-}

import DateTimePicker.Config exposing (Type(..))
import DateTimePicker.DateTime as DateTime
import DateTimePicker.Internal exposing (StateValue, TimeIndicator(..))


updateCurrentDate : Type msg -> StateValue -> Maybe DateTime.DateTime
updateCurrentDate pickerType stateValue =
    let
        updatedDate =
            stateValue.date

        updatedDateTime =
            case ( ( stateValue.date, stateValue.time.hour ), ( stateValue.time.minute, stateValue.time.amPm ) ) of
                ( ( Just date, Just hour ), ( Just minute, Just amPm ) ) ->
                    Just (DateTime.setTime hour minute amPm date)

                _ ->
                    Nothing

        updatedTime =
            case ( stateValue.time.hour, stateValue.time.minute, stateValue.time.amPm ) of
                ( Just hour, Just minute, Just amPm ) ->
                    Just (DateTime.fromTime hour minute amPm)

                _ ->
                    Nothing
    in
    case pickerType of
        DateType _ ->
            updatedDate

        DateTimeType _ ->
            updatedDateTime

        TimeType _ ->
            updatedTime


updateTimeIndicator : StateValue -> StateValue
updateTimeIndicator stateValue =
    case ( ( stateValue.activeTimeIndicator, stateValue.time.hour ), ( stateValue.time.minute, stateValue.time.amPm ) ) of
        ( ( Just HourIndicator, _ ), ( Nothing, _ ) ) ->
            { stateValue | activeTimeIndicator = Just MinuteIndicator }

        ( ( Just HourIndicator, _ ), ( Just _, Nothing ) ) ->
            { stateValue | activeTimeIndicator = Just AMPMIndicator }

        ( ( Just HourIndicator, _ ), ( Just _, Just _ ) ) ->
            { stateValue | activeTimeIndicator = Nothing }

        ( ( Just MinuteIndicator, _ ), ( _, Nothing ) ) ->
            { stateValue | activeTimeIndicator = Just AMPMIndicator }

        ( ( Just MinuteIndicator, Nothing ), ( _, Just _ ) ) ->
            { stateValue | activeTimeIndicator = Just HourIndicator }

        ( ( Just MinuteIndicator, Just _ ), ( _, Just _ ) ) ->
            { stateValue | activeTimeIndicator = Nothing }

        ( ( Just AMPMIndicator, Nothing ), ( _, _ ) ) ->
            { stateValue | activeTimeIndicator = Just HourIndicator }

        ( ( Just AMPMIndicator, Just _ ), ( Nothing, _ ) ) ->
            { stateValue | activeTimeIndicator = Just MinuteIndicator }

        ( ( Just AMPMIndicator, Just _ ), ( Just _, _ ) ) ->
            { stateValue | activeTimeIndicator = Nothing }

        ( ( Nothing, Nothing ), ( _, _ ) ) ->
            { stateValue | activeTimeIndicator = Just HourIndicator }

        ( ( Nothing, Just _ ), ( Nothing, _ ) ) ->
            { stateValue | activeTimeIndicator = Just MinuteIndicator }

        ( ( Nothing, Just _ ), ( Just _, Nothing ) ) ->
            { stateValue | activeTimeIndicator = Just AMPMIndicator }

        ( ( _, Just _ ), ( Just _, Just _ ) ) ->
            { stateValue | activeTimeIndicator = Nothing }
