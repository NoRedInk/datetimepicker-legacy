module DateTimePicker.AnalogClock exposing (clock)

import DateTimePicker.ClockUtils exposing (hours, minutes, minutesPerFive)
import DateTimePicker.Config exposing (Type(..))
import DateTimePicker.DateTime as DateTime
import DateTimePicker.Events exposing (MoveData, onMouseDownPreventDefault, onMouseMoveWithPosition, onPointerMoveWithPosition, onPointerUp, onTouchMovePreventDefault)
import DateTimePicker.Geometry exposing (Point)
import DateTimePicker.Helpers exposing (updateCurrentDate, updateTimeIndicator)
import DateTimePicker.Internal exposing (InternalState(..), StateValue, getStateValue)
import Dict
import Html.Styled as Html exposing (Html, div)
import Json.Decode
import String
import Svg.Styled as Svg exposing (Svg, circle, g, line, svg, text, text_)
import Svg.Styled.Attributes exposing (cx, cy, dominantBaseline, fill, height, r, stroke, strokeWidth, textAnchor, viewBox, width, x, x1, x2, y, y1, y2)


hourArrowLength : Int
hourArrowLength =
    50


minuteArrowLength : Int
minuteArrowLength =
    70


clock : Type msg -> (InternalState -> Maybe DateTime.DateTime -> msg) -> InternalState -> Maybe DateTime.DateTime -> Html msg
clock pickerType onChange state date =
    let
        stateValue =
            getStateValue state
    in
    div
        []
        [ svg
            [ width "200"
            , height "200"
            , viewBox "0 0 200 200"
            ]
            [ circle
                [ cx "100"
                , cy "100"
                , r "100"
                , fill "#eee"
                , onMouseDownPreventDefault (mouseDownHandler pickerType state date onChange)
                , onPointerUp (mouseDownHandler pickerType state date onChange)
                , onMouseMoveWithPosition (mouseOverHandler state date onChange)
                , onTouchMovePreventDefault (onChange state date)
                , onPointerMoveWithPosition (mouseOverHandler state date onChange)
                ]
                []
            , case stateValue.activeTimeIndicator of
                Just DateTimePicker.Internal.MinuteIndicator ->
                    g [] (minutesPerFive |> Dict.toList |> List.map (clockFace pickerType onChange state date))

                _ ->
                    g [] (hours |> Dict.toList |> List.map (clockFace pickerType onChange state date))
            , arrow pickerType onChange state date
            , currentTime pickerType onChange state date
            ]
        ]


currentTime : Type msg -> (InternalState -> Maybe DateTime.DateTime -> msg) -> InternalState -> Maybe DateTime.DateTime -> Svg msg
currentTime pickerType onChange state date =
    let
        stateValue =
            getStateValue state

        time =
            stateValue.time

        drawHour hour minute =
            Dict.get (String.fromInt hour) hours
                |> Maybe.map (\a -> a - (toFloat minute * pi / 360))
                |> Maybe.map (DateTimePicker.Geometry.calculateArrowPoint originPoint hourArrowLength >> drawArrow pickerType onChange state date)
                |> Maybe.withDefault (text "")

        drawMinute minute =
            Dict.get (String.fromInt minute) minutes
                |> Maybe.map (DateTimePicker.Geometry.calculateArrowPoint originPoint minuteArrowLength >> drawArrow pickerType onChange state date)
                |> Maybe.withDefault (text "")
    in
    case ( ( stateValue.activeTimeIndicator, time.hour ), ( time.minute, time.amPm ) ) of
        ( ( Nothing, Just hour ), ( Just minute, Just _ ) ) ->
            g [] [ drawHour hour minute, drawMinute minute ]

        _ ->
            text ""


clockFace : Type msg -> (InternalState -> Maybe DateTime.DateTime -> msg) -> InternalState -> Maybe DateTime.DateTime -> ( String, Float ) -> Svg msg
clockFace pickerType onChange state date ( number, radians ) =
    let
        point =
            DateTimePicker.Geometry.calculateArrowPoint originPoint 85 radians
    in
    text_
        [ x <| String.fromInt point.x
        , y <| String.fromInt point.y
        , textAnchor "middle"
        , dominantBaseline "central"
        , onMouseDownPreventDefault (mouseDownHandler pickerType state date onChange)
        , onPointerUp (mouseDownHandler pickerType state date onChange)
        ]
        [ text number ]


originPoint : Point
originPoint =
    Point 100 100


axisPoint : Point
axisPoint =
    Point 200 100


arrow : Type msg -> (InternalState -> Maybe DateTime.DateTime -> msg) -> InternalState -> Maybe DateTime.DateTime -> Svg msg
arrow pickerType onChange state date =
    let
        stateValue =
            getStateValue state

        length =
            case stateValue.activeTimeIndicator of
                Just DateTimePicker.Internal.HourIndicator ->
                    hourArrowLength

                Just DateTimePicker.Internal.MinuteIndicator ->
                    minuteArrowLength

                _ ->
                    0

        arrowPoint angle =
            angle
                |> DateTimePicker.Geometry.calculateArrowPoint originPoint length

        isJust maybe =
            case maybe of
                Just _ ->
                    True

                Nothing ->
                    False

        shouldDrawArrow =
            case stateValue.activeTimeIndicator of
                Just DateTimePicker.Internal.HourIndicator ->
                    isJust stateValue.time.hour

                Just DateTimePicker.Internal.MinuteIndicator ->
                    isJust stateValue.time.minute

                _ ->
                    False
    in
    case stateValue.currentAngle of
        Nothing ->
            text ""

        Just angle ->
            if shouldDrawArrow then
                angle
                    |> arrowPoint
                    |> drawArrow pickerType onChange state date

            else
                text ""


drawArrow : Type msg -> (InternalState -> Maybe DateTime.DateTime -> msg) -> InternalState -> Maybe DateTime.DateTime -> Point -> Svg msg
drawArrow pickerType onChange state date point =
    line
        [ x1 "100"
        , y1 "100"
        , x2 <| String.fromInt point.x
        , y2 <| String.fromInt point.y
        , strokeWidth "2px"
        , stroke "#aaa"
        , onMouseDownPreventDefault (mouseDownHandler pickerType state date onChange)
        , onPointerUp (mouseDownHandler pickerType state date onChange)
        ]
        []


mouseDownHandler : Type msg -> InternalState -> Maybe DateTime.DateTime -> (InternalState -> Maybe DateTime.DateTime -> msg) -> msg
mouseDownHandler pickerType state date onChange =
    let
        stateValue =
            getStateValue state

        updatedDate =
            updateCurrentDate pickerType stateValue

        updatedStateValue =
            case ( updatedDate, stateValue.activeTimeIndicator ) of
                ( Just _, _ ) ->
                    { stateValue | event = "analog.mouseDownHandler", activeTimeIndicator = Nothing, currentAngle = Nothing }

                ( _, Just DateTimePicker.Internal.HourIndicator ) ->
                    { stateValue | event = "analog.mouseDownHandler", activeTimeIndicator = Just DateTimePicker.Internal.MinuteIndicator, currentAngle = Nothing }

                ( _, Just DateTimePicker.Internal.MinuteIndicator ) ->
                    { stateValue | event = "analog.mouseDownHandler", activeTimeIndicator = Just DateTimePicker.Internal.AMPMIndicator, currentAngle = Nothing }

                _ ->
                    { stateValue | event = "analog.mouseDownHandler", activeTimeIndicator = Just DateTimePicker.Internal.HourIndicator, currentAngle = Nothing }
    in
    onChange
        (InternalState <| updateTimeIndicator stateValue)
        updatedDate


mouseOverHandler : InternalState -> Maybe DateTime.DateTime -> (InternalState -> Maybe DateTime.DateTime -> msg) -> MoveData -> Json.Decode.Decoder msg
mouseOverHandler state date onChange moveData =
    let
        stateValue =
            getStateValue state

        decoder updatedState =
            Json.Decode.succeed (onChange updatedState date)
    in
    case stateValue.activeTimeIndicator of
        Just DateTimePicker.Internal.HourIndicator ->
            decoder (updateHourState stateValue date moveData)

        Just DateTimePicker.Internal.MinuteIndicator ->
            decoder (updateMinuteState stateValue date moveData)

        _ ->
            decoder (InternalState stateValue)


updateHourState : StateValue -> Maybe DateTime.DateTime -> MoveData -> InternalState
updateHourState stateValue date moveData =
    let
        currentAngle =
            DateTimePicker.Geometry.calculateAngle originPoint axisPoint (Point moveData.offsetX moveData.offsetY)

        closestHour =
            hours
                |> Dict.toList
                |> List.map (\( hour, radians ) -> ( ( hour, radians ), abs (radians - currentAngle) ))
                |> List.sortBy Tuple.second
                |> List.head
                |> Maybe.map Tuple.first

        updateTime time hour =
            { time | hour = hour |> Maybe.andThen String.toInt }
    in
    InternalState
        { stateValue
            | currentAngle =
                Maybe.map Tuple.second closestHour
            , time = updateTime stateValue.time (Maybe.map Tuple.first closestHour)
        }


updateMinuteState : StateValue -> Maybe DateTime.DateTime -> MoveData -> InternalState
updateMinuteState stateValue date moveData =
    let
        currentAngle =
            DateTimePicker.Geometry.calculateAngle originPoint axisPoint (Point moveData.offsetX moveData.offsetY)

        closestMinute =
            minutes
                |> Dict.toList
                |> List.map (\( minute, radians ) -> ( ( minute, radians ), abs (radians - currentAngle) ))
                |> List.sortBy Tuple.second
                |> List.head
                |> Maybe.map Tuple.first

        updateTime time minute =
            { time | minute = minute |> Maybe.andThen String.toInt }
    in
    InternalState
        { stateValue
            | currentAngle =
                Maybe.map Tuple.second closestMinute
            , time = updateTime stateValue.time (Maybe.map Tuple.first closestMinute)
        }
